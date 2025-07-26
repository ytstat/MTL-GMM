library(dplyr)
library(mclust)
library(doParallel)
library(MASS)
library(caret)
library(mtlgmm)
library(gtools)
library(DirichletReg)

elative_path <- here()

Sys.setenv(LANG = "en_US.UTF-8")
seed <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
cat("seed=", seed, "\n")


filename = paste(relative_path, "/output/simulation/har_multi/", seed, ".RData", sep = "")
if (file.exists(filename)) {
  stop("Done!")
}

set.seed(seed, kind = "L'Ecuyer-CMRG")
if(Sys.getenv("SLURM_CPUS_PER_TASK") != "") {
  ncores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK"))
} else {
  ncores <- detectCores()
}

# ---------------------------------------------------------
# Real-data study: Human activity recognition, multi-class clustering
# ---------------------------------------------------------
# See Sections 3.2 and S.5.2.1 in the paper

D_train <- read.table("X_train.txt")
D_y <- read.table("y_train.txt")
D_id <- read.table("subject_train.txt")
D_test <- read.table("X_test.txt")
D_y_test <- read.table("y_test.txt")
D_id_test <- read.table("subject_test.txt")


D_train <- rbind(D_train, D_test) %>% mutate(id = c(as.numeric(unlist(D_id)), as.numeric(unlist(D_id_test))), y = c(as.numeric(unlist(D_y)), as.numeric(unlist(D_y_test))))

K0 <- length(unique(D_train$id))


x <- sapply(1:length(unique(D_train$id)[1:K0]), function(k){
  D_train %>% filter(id == unique(D_train$id)[k]) %>% dplyr::select(-id, -y)
}, simplify = FALSE)
y <- sapply(1:length(unique(D_train$id)[1:K0]), function(k){
  a <- D_train %>% filter(id == unique(D_train$id)[k]) %>% dplyr::select(y) %>% unlist(.) %>% as.numeric(.)
  a
}, simplify = FALSE)

R <- 6
K <- length(unique(D_train$id)[1:K0])
p <- 15

train_id <- sapply(1:length(x), function(k){
  sample(1:nrow(x[[k]]), floor(nrow(x[[k]])*0.7))
}, simplify = FALSE)

pca_train <- sapply(1:length(x), function(k){
  prcomp(x[[k]][train_id[[k]], ])
}, simplify = FALSE)

x_train <- sapply(1:length(x), function(k){
  pca_train[[k]]$x[, 1:p]
}, simplify = FALSE)

y_train <- sapply(1:length(x), function(k){
  y[[k]][train_id[[k]]]
}, simplify = FALSE)

x_test <- sapply(1:length(x), function(k){
  predict(pca_train[[k]], newdata = x[[k]][-train_id[[k]], ])[, 1:p]
}, simplify = FALSE)

y_test <- sapply(1:length(x), function(k){
  y[[k]][-train_id[[k]]]
}, simplify = FALSE)

x_train_std <- sapply(1:K, function(k){
  scale(x_train[[k]])
}, simplify = FALSE)


x_test_std <- sapply(1:K, function(k){
  scale(x_test[[k]], center = attr(x_train_std[[k]], "scaled:center"), scale = attr(x_train_std[[k]], "scaled:scale"))
}, simplify = FALSE)


# Single-task GMM
fitted_values <- initialize_multi(x = x_train_std, method = "EM", R = R)
pihat <- alignment_multi(fitted_values$mu, fitted_values$Sigma, method = "greedy", num_replication = min(50, factorial(K)), ncores = ncores)
fitted_values <- alignment_swap_multi(pihat, initial_value_list = fitted_values)

y_pred_single <- sapply(1:length(x), function(k){
  mu_k <- sapply(1:R, function(r){
    fitted_values$mu[[r]][, k]
  })
  beta_k <- sapply(1:R, function(r){
    solve(fitted_values$Sigma[[k]]) %*% (mu_k[, r] - mu_k[, 1])
  })
  predict_gmm_multi(w = fitted_values$w[, k], mu = mu_k, beta = beta_k, newx = x_test_std[[k]])
}, simplify = FALSE)


# Pooled-GMM
x.comb <- Reduce("rbind", x_train_std)
fit_pooled <- Mclust(x.comb, G = R, modelNames = "EEE")
fitted_values_pooled <- list(w = NULL, mu = NULL, beta = NULL, Sigma = NULL, delta = NULL)
fitted_values_pooled$w <- matrix(rep(fit_pooled$parameters$pro,K), ncol = K)
fitted_values_pooled$mu <- sapply(1:R, function(r){
  matrix(fit_pooled$parameters$mean[, r], ncol = K, nrow = p)
}, simplify = FALSE)
fitted_values_pooled$Sigma <- sapply(1:K, function(k){
  fit_pooled$parameters$variance$Sigma
}, simplify = FALSE)
fitted_values_pooled$beta <- sapply(1:R, function(r){
  if (r == 1) {
    matrix(0, nrow = p, ncol = K)
  } else {
    sapply(1:K, function(k){
      solve(fit_pooled$parameters$variance$Sigma)%*% (fit_pooled$parameters$mean[,r] - fit_pooled$parameters$mean[,1])
    })
  }
}, simplify = FALSE)
fitted_values_pooled$delta <- t(sapply(1:R, function(r){
  if (r == 1) {
    numeric(K)
  } else {
    sapply(1:K, function(k){
      sum(fitted_values_pooled$beta[[r]][, k]*(fitted_values_pooled$mu[[r]][, k] + fitted_values_pooled$mu[[1]][, k])/2)
    })
  }
}))
y_pred_pooling <- sapply(1:K, function(k){
  mu_k <- sapply(1:R, function(r){
    fitted_values_pooled$mu[[r]][, k]
  })
  beta_k <- sapply(1:R, function(r){
    solve(fitted_values_pooled$Sigma[[k]]) %*% (mu_k[, r] - mu_k[, 1])
  })
  predict_gmm_multi(w = fitted_values_pooled$w[, k], mu = mu_k, beta = beta_k, newx = x_test_std[[k]])
}, simplify = FALSE)


# MTL-GMM
fit <- mtlgmm_multi(x = x_train_std, kappa = 1/3, initial_method = "EM", ncores = ncores, cv_nfolds = 10, cv_upper = "auto", cv_length = 10,
                    lambda_choice = "cv", step_size = "lipschitz", tol = 1e-3, C_lambda = 5, R = R, alignment_method = "greedy",
                    num_replication = 50)


y_pred_mtlgmm <- sapply(1:K, function(k){
  mu_k <- sapply(1:R, function(r){
    fit$mu[[r]][, k]
  })
  beta_k <- sapply(1:R, function(r){
    solve(fit$Sigma[[k]]) %*% (mu_k[, r] - mu_k[, 1])
  })
  predict_gmm_multi(w = fit$w[, k], mu = mu_k, beta = beta_k, newx = x_test_std[[k]])
}, simplify = FALSE)

error <- c(misclustering_error_multi(y_pred_single, y_test, R = R, type = "max"),
           misclustering_error_multi(y_pred_mtlgmm, y_test, R = R, type = "max"),
           misclustering_error_multi(y_pred_pooling, y_test, R = R, type = "max"),
           misclustering_error_multi(y_pred_single, y_test, R = R, type = "avg"),
           misclustering_error_multi(y_pred_mtlgmm, y_test, R = R, type = "avg"),
           misclustering_error_multi(y_pred_pooling, y_test, R = R, type = "avg"))


error_all <- rbind(misclustering_error_multi(y_pred_single, y_test, R = R, type = "all"),
                   misclustering_error_multi(y_pred_mtlgmm, y_test, R = R, type = "all"),
                   misclustering_error_multi(y_pred_pooling, y_test, R = R, type = "all"))
print(error)

# -------------------------------------------------------
save(error, error_all, file = filename)



