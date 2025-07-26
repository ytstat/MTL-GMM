library(dplyr)
library(mclust)
library(doParallel)
library(MASS)
library(caret)
library(mtlgmm)
library(gtools)
library(DirichletReg)
library(aricode)


relative_path <- here()

Sys.setenv(LANG = "en_US.UTF-8")
seed <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
cat("seed=", seed, "\n")


filename = paste(relative_path, "/output/simulation/misspecified_em/", seed, ".RData", sep = "")
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
# Simulation 1 with different choices of cluster numbers in MTL-GMM
# ---------------------------------------------------------
# See Figure S.5 and Section S.3.2 in the paper

h_list <- seq(0, 10, 1)

K <- 10
p <- 15

nmi_df <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(nmi_df) <- c("NMI", "method", "h")


for (i in 1:length(h_list)) {
  data_bundle <- data_generation(K = K, outlier_K = 0, h = h_list[i], n = 300*2, p = 15, simulation_no = "MTL-1") #mu_shift = 1

  x_train <- sapply(1:K, function(k){
    data_bundle$data$x[[k]][1:100,]
  }, simplify = FALSE)
  x_test <- sapply(1:K, function(k){
    data_bundle$data$x[[k]][-(1:100),]
  }, simplify = FALSE)
  y_test <- sapply(1:K, function(k){
    data_bundle$data$y[[k]][-(1:100)]
  }, simplify = FALSE)


  # Single-task GMM
  x <- x_train

  fitted_values2 <- initialize_multi(x, "EM", R = 2)
  pihat <- alignment_multi(fitted_values2$mu, fitted_values2$Sigma, method = "greedy", num_replication = 50, ncores = ncores)
  fitted_values2 <- alignment_swap_multi(pihat, initial_value_list = fitted_values2)

  fitted_values3 <- initialize_multi(x, "EM", R = 3)
  pihat <- alignment_multi(fitted_values3$mu, fitted_values3$Sigma, method = "greedy", num_replication = 50, ncores = ncores)
  fitted_values3 <- alignment_swap_multi(pihat, initial_value_list = fitted_values3)

  fitted_values4 <- initialize_multi(x, "EM", R = 4)
  pihat <- alignment_multi(fitted_values4$mu, fitted_values4$Sigma, method = "greedy", num_replication = 50, ncores = ncores)
  fitted_values4 <- alignment_swap_multi(pihat, initial_value_list = fitted_values4)

  fitted_values5 <- initialize_multi(x, "EM", R = 5)
  pihat <- alignment_multi(fitted_values5$mu, fitted_values5$Sigma, method = "greedy", num_replication = 50, ncores = ncores)
  fitted_values5 <- alignment_swap_multi(pihat, initial_value_list = fitted_values5)


  # MTL-GMM
  fit2 <- mtlgmm_multi(x = x, kappa = 1/3, initial_method = "EM", ncores = ncores, cv_nfolds = 10, cv_upper = "auto", cv_length = 10,
                          lambda_choice = "cv", step_size = "lipschitz", tol = 1e-3, C_lambda = 5, R = 2, alignment_method = "greedy", num_replication = 50)
  fit3 <- mtlgmm_multi(x = x, kappa = 1/3, initial_method = "EM", ncores = ncores, cv_nfolds = 10, cv_upper = "auto", cv_length = 10,
                          lambda_choice = "cv", step_size = "lipschitz", tol = 1e-3, C_lambda = 5, R = 3, alignment_method = "greedy", num_replication = 50)
  fit4 <- mtlgmm_multi(x = x, kappa = 1/3, initial_method = "EM", ncores = ncores, cv_nfolds = 10, cv_upper = "auto", cv_length = 10,
                          lambda_choice = "cv", step_size = "lipschitz", tol = 1e-3, C_lambda = 5, R = 4, alignment_method = "greedy", num_replication = 50)
  fit5 <- mtlgmm_multi(x = x, kappa = 1/3, initial_method = "EM", ncores = ncores, cv_nfolds = 10, cv_upper = "auto", cv_length = 10,
                          lambda_choice = "cv", step_size = "lipschitz", tol = 1e-3, C_lambda = 5, R = 5, alignment_method = "greedy", num_replication = 50)


  y_pred_single2 <- sapply(1:K, function(k){
    mu_k <- sapply(1:2, function(r){
      fitted_values2$mu[[r]][, k]
    })
    beta_k <- sapply(1:2, function(r){
      solve(fitted_values2$Sigma[[k]]) %*% (mu_k[, r] - mu_k[, 1])
    })
    predict_gmm_multi(w = fitted_values2$w[, k], mu = mu_k, beta = beta_k, newx = x_test[[k]])
  }, simplify = FALSE)

  y_pred_single3 <- sapply(1:K, function(k){
    mu_k <- sapply(1:3, function(r){
      fitted_values3$mu[[r]][, k]
    })
    beta_k <- sapply(1:3, function(r){
      solve(fitted_values3$Sigma[[k]]) %*% (mu_k[, r] - mu_k[, 1])
    })
    predict_gmm_multi(w = fitted_values3$w[, k], mu = mu_k, beta = beta_k, newx = x_test[[k]])
  }, simplify = FALSE)

  y_pred_single4 <- sapply(1:K, function(k){
    mu_k <- sapply(1:4, function(r){
      fitted_values4$mu[[r]][, k]
    })
    beta_k <- sapply(1:4, function(r){
      solve(fitted_values4$Sigma[[k]]) %*% (mu_k[, r] - mu_k[, 1])
    })
    predict_gmm_multi(w = fitted_values4$w[, k], mu = mu_k, beta = beta_k, newx = x_test[[k]])
  }, simplify = FALSE)

  y_pred_single5 <- sapply(1:K, function(k){
    mu_k <- sapply(1:5, function(r){
      fitted_values5$mu[[r]][, k]
    })
    beta_k <- sapply(1:5, function(r){
      solve(fitted_values5$Sigma[[k]]) %*% (mu_k[, r] - mu_k[, 1])
    })
    predict_gmm_multi(w = fitted_values5$w[, k], mu = mu_k, beta = beta_k, newx = x_test[[k]])
  }, simplify = FALSE)

  y_pred_mtlgmm2 <- sapply(1:K, function(k){
    mu_k <- sapply(1:2, function(r){
      fit2$mu[[r]][, k]
    })
    beta_k <- sapply(1:2, function(r){
      fit2$beta[[r]][, k]
    })
    predict_gmm_multi(w = fit2$w[, k], mu = mu_k, beta = beta_k, newx = x_test[[k]])
  }, simplify = FALSE)

  y_pred_mtlgmm3 <- sapply(1:K, function(k){
    mu_k <- sapply(1:3, function(r){
      fit3$mu[[r]][, k]
    })
    beta_k <- sapply(1:3, function(r){
      fit3$beta[[r]][, k]
    })
    predict_gmm_multi(w = fit3$w[, k], mu = mu_k, beta = beta_k, newx = x_test[[k]])
  }, simplify = FALSE)

  y_pred_mtlgmm4 <- sapply(1:K, function(k){
    mu_k <- sapply(1:4, function(r){
      fit4$mu[[r]][, k]
    })
    beta_k <- sapply(1:4, function(r){
      fit4$beta[[r]][, k]
    })
    predict_gmm_multi(w = fit4$w[, k], mu = mu_k, beta = beta_k, newx = x_test[[k]])
  }, simplify = FALSE)

  y_pred_mtlgmm5 <- sapply(1:K, function(k){
    mu_k <- sapply(1:5, function(r){
      fit5$mu[[r]][, k]
    })
    beta_k <- sapply(1:5, function(r){
      fit5$beta[[r]][, k]
    })
    predict_gmm_multi(w = fit5$w[, k], mu = mu_k, beta = beta_k, newx = x_test[[k]])
  }, simplify = FALSE)


  new_result <- data.frame(rbind(c(mean(sapply(1:K, function(k){NMI(y_test[[k]], y_pred_single2[[k]], variant = "sqrt")})), "Single-task-GMM-2"),
                                 c(mean(sapply(1:K, function(k){NMI(y_test[[k]], y_pred_single3[[k]], variant = "sqrt")})), "Single-task-GMM-3"),
                                 c(mean(sapply(1:K, function(k){NMI(y_test[[k]], y_pred_single4[[k]], variant = "sqrt")})), "Single-task-GMM-4"),
                                 c(mean(sapply(1:K, function(k){NMI(y_test[[k]], y_pred_single5[[k]], variant = "sqrt")})), "Single-task-GMM-5"),
                                 c(mean(sapply(1:K, function(k){NMI(y_test[[k]], y_pred_mtlgmm2[[k]], variant = "sqrt")})), "MTL-GMM-2"),
                                 c(mean(sapply(1:K, function(k){NMI(y_test[[k]], y_pred_mtlgmm3[[k]], variant = "sqrt")})), "MTL-GMM-3"),
                                 c(mean(sapply(1:K, function(k){NMI(y_test[[k]], y_pred_mtlgmm4[[k]], variant = "sqrt")})), "MTL-GMM-4"),
                                 c(mean(sapply(1:K, function(k){NMI(y_test[[k]], y_pred_mtlgmm5[[k]], variant = "sqrt")})), "MTL-GMM-5")))
  new_result <- cbind(new_result, h_list[i])
  new_result[, 1] <- as.numeric(new_result[, 1])
  colnames(new_result) <- c("NMI", "method", "h")
  nmi_df <- rbind(nmi_df, cbind(new_result))

}


save(nmi_df, file = filename)




