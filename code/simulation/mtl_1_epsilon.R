library(dplyr)
library(mclust)
library(doParallel)
library(MASS)
library(caret)
library(mtlgmm)
library(gtools)
library(DirichletReg)
library(igraph)

relative_path <- here()

Sys.setenv(LANG = "en_US.UTF-8")
seed <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
cat("seed=", seed, "\n")


filename = paste(relative_path, "/output/simulation/mtl_1_epsilon/", seed, ".RData", sep = "")
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
# Simulation 4 of MTL in the paper, the same setting as Simulation 1 but with varying epsilon values
# ---------------------------------------------------------
# See Section S.5.1.4 in the paper

h <- seq(0, 10, 1)
K <- 10
epsilon <- seq(0, 0.9, 0.1)
C_matrix <- as.matrix(expand.grid(h, epsilon))
colnames(C_matrix) <- c("h", "epsilon")


error_single <- matrix(nrow = nrow(C_matrix), ncol = 7, dimnames = list(NULL, c("w", "mu", "beta", "delta", "Sigma", "misclustering_max", "misclustering_avg")))
error_mtlgmm <- matrix(nrow = nrow(C_matrix), ncol = 7, dimnames = list(NULL, c("w", "mu", "beta", "delta", "Sigma", "misclustering_max", "misclustering_avg")))
error_pooling <- matrix(nrow = nrow(C_matrix), ncol = 7, dimnames = list(NULL, c("w", "mu", "beta", "delta", "Sigma", "misclustering_max", "misclustering_avg")))


for (i in 1:nrow(C_matrix)) {
  data_bundle <- data_generation(K = K, outlier_K = floor(C_matrix[i, 2]*K), h = C_matrix[i, 1], n = 600, p = 15, simulation_no = "MTL-1")

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
  fitted_values <- initialize(x, "EM")
  r <- alignment(fitted_values$beta, method = "exhaustive")
  fitted_values <- alignment_swap(r, initial_value_list = fitted_values)


  # MTL-GMM
  fit <- mtlgmm(x = x, kappa = 1/3, initial_method = "EM", ncores = ncores, cv_nfolds = 10, cv_upper = "auto", cv_lower = 0.01, cv_length = 10,
                lambda_choice = "cv", step_size = "lipschitz", tol = 1e-3, trim = C_matrix[i, 2], cv_criterion = ifelse(C_matrix[i, 2] < 0.5, "trimmed_mean", "median"))

  # Pooled-GMM
  x.comb <- Reduce("rbind", x)
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


  if (C_matrix[i, "epsilon"] == 0) {
    data_bundle$data$outlier_index <- 11
  }

  error_single[i, "w"] <- estimation_error(fitted_values$w[-data_bundle$data$outlier_index], data_bundle$parameter$w[-data_bundle$data$outlier_index], "w")
  error_pooling[i, "w"] <- estimation_error(fitted_values_pooled$w[-data_bundle$data$outlier_index], data_bundle$parameter$w[-data_bundle$data$outlier_index], "w")
  error_mtlgmm[i, "w"] <- estimation_error(fit$w[-data_bundle$data$outlier_index], data_bundle$parameter$w[-data_bundle$data$outlier_index], "w")

  error_single[i, "mu"] <- estimation_error(list(fitted_values$mu1[, -data_bundle$data$outlier_index, drop = F], fitted_values$mu2[, -data_bundle$data$outlier_index, drop = F]), list(data_bundle$parameter$mu1[, -data_bundle$data$outlier_index, drop = F], data_bundle$parameter$mu2[, -data_bundle$data$outlier_index, drop = F]), "mu")
  error_pooling[i, "mu"] <- estimation_error(list(fitted_values_pooled$mu1[, -data_bundle$data$outlier_index, drop = F], fitted_values_pooled$mu2[, -data_bundle$data$outlier_index, drop = F]), list(data_bundle$parameter$mu1[, -data_bundle$data$outlier_index, drop = F], data_bundle$parameter$mu2[, -data_bundle$data$outlier_index, drop = F]), "mu")
  error_mtlgmm[i, "mu"] <- estimation_error(list(fit$mu1[, -data_bundle$data$outlier_index, drop = F], fit$mu2[, -data_bundle$data$outlier_index, drop = F]), list(data_bundle$parameter$mu1[, -data_bundle$data$outlier_index, drop = F], data_bundle$parameter$mu2[, -data_bundle$data$outlier_index, drop = F]), "mu")

  error_single[i, "beta"] <- estimation_error(fitted_values$beta[, -data_bundle$data$outlier_index, drop = F], data_bundle$parameter$beta[, -data_bundle$data$outlier_index, drop = F], "beta")
  error_pooling[i, "beta"] <- estimation_error(fitted_values_pooled$beta[, -data_bundle$data$outlier_index, drop = F], data_bundle$parameter$beta[, -data_bundle$data$outlier_index, drop = F], "beta")
  error_mtlgmm[i, "beta"] <- estimation_error(fit$beta[, -data_bundle$data$outlier_index, drop = F], data_bundle$parameter$beta[, -data_bundle$data$outlier_index, drop = F], "beta")

  error_single[i, "delta"] <- estimation_error(fitted_values$delta[-data_bundle$data$outlier_index], data_bundle$parameter$delta[-data_bundle$data$outlier_index], "delta")
  error_pooling[i, "delta"] <- estimation_error(fitted_values_pooled$delta[-data_bundle$data$outlier_index], data_bundle$parameter$delta[-data_bundle$data$outlier_index], "delta")
  error_mtlgmm[i, "delta"] <- estimation_error(fit$delta[-data_bundle$data$outlier_index], data_bundle$parameter$delta[-data_bundle$data$outlier_index], "delta")


  error_single[i, "Sigma"] <- estimation_error(fitted_values$Sigma[-data_bundle$data$outlier_index], data_bundle$parameter$Sigma[-data_bundle$data$outlier_index], "Sigma")
  error_pooling[i, "Sigma"] <- estimation_error(fitted_values_pooled$Sigma[-data_bundle$data$outlier_index], data_bundle$parameter$Sigma[-data_bundle$data$outlier_index], "Sigma")
  error_mtlgmm[i, "Sigma"] <- estimation_error(fit$Sigma[-data_bundle$data$outlier_index], data_bundle$parameter$Sigma[-data_bundle$data$outlier_index], "Sigma")


  y_pred_single <- sapply(1:K, function(k){
    predict_gmm(w = fitted_values$w[k], mu1 = fitted_values$mu1[, k], mu2 = fitted_values$mu2[, k], beta = fitted_values$beta[, k], newx = x_test[[k]])
  }, simplify = FALSE)
  y_pred_pooling <- sapply(1:K, function(k){
    predict_gmm(w = fitted_values_pooled$w[k], mu1 = fitted_values_pooled$mu1[, k], mu2 = fitted_values_pooled$mu2[, k], beta = fitted_values_pooled$beta[, k], newx = x_test[[k]])
  }, simplify = FALSE)
  y_pred_mtlgmm <- sapply(1:K, function(k){
    predict_gmm(w = fit$w[k], mu1 = fit$mu1[, k], mu2 = fit$mu2[, k], beta = fit$beta[, k], newx = x_test[[k]])
  }, simplify = FALSE)

  error_single[i, "misclustering_max"] <- misclustering_error(y_pred_single[-data_bundle$data$outlier_index], y_test[-data_bundle$data$outlier_index], "max")
  error_pooling[i, "misclustering_max"] <- misclustering_error(y_pred_pooling[-data_bundle$data$outlier_index], y_test[-data_bundle$data$outlier_index], "max")
  error_mtlgmm[i, "misclustering_max"] <- misclustering_error(y_pred_mtlgmm[-data_bundle$data$outlier_index], y_test[-data_bundle$data$outlier_index], "max")

  error_single[i, "misclustering_avg"] <- misclustering_error(y_pred_single[-data_bundle$data$outlier_index], y_test[-data_bundle$data$outlier_index], "avg")
  error_pooling[i, "misclustering_avg"] <- misclustering_error(y_pred_pooling[-data_bundle$data$outlier_index], y_test[-data_bundle$data$outlier_index], "avg")
  error_mtlgmm[i, "misclustering_avg"] <- misclustering_error(y_pred_mtlgmm[-data_bundle$data$outlier_index], y_test[-data_bundle$data$outlier_index], "avg")

}


save(error_single, error_pooling, error_mtlgmm, file = filename)




