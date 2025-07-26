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


filename = paste(relative_path, "/output/simulation/pen/", seed, ".RData", sep = "")
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
# Real-data study: Pen-based recognition of handwritten digits, binary clustering
# ---------------------------------------------------------
# See Section S.5.2.2 in the paper


load("info_test.RData")
D_test <- read.csv("pendigits.tes", header = FALSE)
load("info_train.RData")
D_train <- read.csv("pendigits.tra", header = FALSE)

colnames(D_test)[ncol(D_test)] <- "class"
D_test <- D_test[, colnames(D_test) != "class"]
K_test <- length(unique(info_test[, "user"]))
x_test <- sapply(1:K_test, function(k){
  D_test[info_test[, "user"] == k, ]
}, simplify = FALSE)
y_test <- sapply(1:K_test, function(k){
  info_test[info_test[, "user"] == k, "class"]+1
}, simplify = FALSE)


colnames(D_train)[ncol(D_train)] <- "class"
D_train <- D_train[, colnames(D_train) != "class"]
K_train <- length(unique(info_train[, "user"]))
x_train <- sapply(1:K_train, function(k){
  D_train[info_train[, "user"] == k, ]
}, simplify = FALSE)
y_train <- sapply(1:K_train, function(k){
  info_train[info_train[, "user"] == k, "class"]+1
}, simplify = FALSE)


# combination
x <- c(x_train, x_test)
y <- c(y_train, y_test)
K <- K_train + K_test

# further selection
label_set <- c(7, 10)
R <- length(label_set)
x <- sapply(1:K, function(k){
  x[[k]][y[[k]] %in% label_set, ]
}, simplify = FALSE)
p <- ncol(x[[1]])

y <- sapply(1:K, function(k){
  a <- y[[k]][y[[k]] %in% label_set]
  for (i in 1:length(label_set)) {
    a[a == label_set[i]] <- i
  }
  a
}, simplify = FALSE)


n_list <- sapply(1:K, function(k){
  nrow(x[[k]])
})

train_id <- sapply(1:K, function(k){
  sample(n_list[k], size = floor(0.9*n_list[k]), replace = FALSE)
}, simplify = FALSE)


x_train <- sapply(1:K, function(k){
  x[[k]][train_id[[k]], ]
}, simplify = FALSE)

x_test <- sapply(1:K, function(k){
  x[[k]][-train_id[[k]], ]
}, simplify = FALSE)


# prevent singularity
for (k in 1:K) {
  for (j in 1:p) {
    if (sd(x[[k]][train_id[[k]], j]) <= 1e-2 || length(unique(x[[k]][train_id[[k]], j])) <= 3) {
      x[[k]][train_id[[k]], j] <- x[[k]][train_id[[k]], j] + rnorm(nrow(x[[k]][train_id[[k]], ]), sd = 1e-2)
    }
  }
}

x_center <- sapply(1:K, function(k){
  x_mean <- colMeans(x[[k]][train_id[[k]], ])
  rep(c(mean(x_mean[seq(1,16,2)]), mean(x_mean[seq(2,16,2)])), 8)
})


x_train_std <- sapply(1:K, function(k){
  scale(x[[k]][train_id[[k]], ], scale = TRUE)
}, simplify = FALSE)

x_test_std <- sapply(1:K, function(k){
  scale(x[[k]][-train_id[[k]], ], center = attr(x_train_std[[k]], "scaled:center"), scale = attr(x_train_std[[k]], "scaled:scale"))
}, simplify = FALSE)



y_test <- sapply(1:K, function(k){
  y[[k]][-train_id[[k]]]
}, simplify = FALSE)


# Single-task GMM
fitted_values <- initialize(x = x_train_std, method = "EM")
pihat <- alignment(fitted_values$beta, method = "greedy")
fitted_values <- alignment_swap(pihat, initial_value_list = fitted_values)

y_pred_single <- sapply(1:K, function(k){
  predict_gmm(w = fitted_values$w[k], mu1 = fitted_values$mu1[, k], mu2 = fitted_values$mu2[, k], beta = fitted_values$beta[, k], newx = x_test_std[[k]])
}, simplify = FALSE)


# Pooling
x.comb <- Reduce("rbind", x_train_std)
fit_pooled <- Mclust(x.comb, G = 2, modelNames = "EEE")
fitted_values_pooled <- list(w = NULL, mu1 = NULL, mu2 = NULL, beta = NULL, Sigma = NULL, delta = NULL)
fitted_values_pooled$w <- rep(fit_pooled$parameters$pro[2], K)
fitted_values_pooled$mu1 <- matrix(rep(fit_pooled$parameters$mean[,1], K), ncol = K)
fitted_values_pooled$mu2 <- matrix(rep(fit_pooled$parameters$mean[,2], K), ncol = K)
fitted_values_pooled$Sigma <- sapply(1:K, function(k){
  fit_pooled$parameters$variance$Sigma
}, simplify = FALSE)
fitted_values_pooled$beta <- sapply(1:K, function(k){
  solve(fit_pooled$parameters$variance$Sigma)%*% (fit_pooled$parameters$mean[,2] - fit_pooled$parameters$mean[,1])
})
fitted_values_pooled$delta <- sapply(1:K, function(k){
  sum(fitted_values_pooled$beta[, k]*(fitted_values_pooled$mu1[, k] + fitted_values_pooled$mu2[, k])/2)
})
y_pred_pooling <- sapply(1:K, function(k){
  predict_gmm(w = fitted_values_pooled$w[k], mu1 = fitted_values_pooled$mu1[, k], mu2 = fitted_values_pooled$mu2[, k], beta = fitted_values_pooled$beta[, k], newx = x_test_std[[k]])
}, simplify = FALSE)


# MTL-GMM
fit <- mtlgmm(x = x_train_std, kappa = 1/3, initial_method = "EM", ncores = ncores, cv_nfolds =10, cv_upper = "auto",
              lambda_choice = "cv", step_size = "lipschitz", tol = 1e-3, trim = 0.1)

y_pred_mtlgmm <- sapply(1:K, function(k){
  predict_gmm(w = fit$w[k], mu1 = fit$mu1[, k], mu2 = fit$mu2[, k], beta = fit$beta[, k], newx = x_test_std[[k]])
}, simplify = FALSE)

error <- c(misclustering_error(y_pred_single, y_test, type = "max"),
           misclustering_error(y_pred_mtlgmm, y_test, type = "max"),
           misclustering_error(y_pred_pooling, y_test, type = "max"),
           misclustering_error(y_pred_single, y_test, type = "avg"),
           misclustering_error(y_pred_mtlgmm, y_test, type = "avg"),
           misclustering_error(y_pred_pooling, y_test, type = "avg"))

error_all <- rbind(misclustering_error(y_pred_single, y_test, type = "all"),
                   misclustering_error(y_pred_mtlgmm, y_test, type = "all"),
                   misclustering_error(y_pred_pooling, y_test, type = "all"))
print(error)

# -------------------------------------------------------
save(error, error_all, file = filename)
