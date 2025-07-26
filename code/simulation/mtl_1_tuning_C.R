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


filename = paste(relative_path, "/output/simulation/mtl_1_tuning_C/", seed, ".RData", sep = "")
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
# Simulation 1 with different C_lambda parameters used in MTL-GMM
# ---------------------------------------------------------
# See Section S.5.1.7 in the paper

h <- seq(0, 10, 1)
outlier_num <- 0:2
C_matrix <- as.matrix(expand.grid(h, outlier_num))
colnames(C_matrix) <- c("h", "outlier_num")
C_lambda_list <- exp(seq(log(0.1), log(10), length.out = 10))

error_single <- matrix(nrow = nrow(C_matrix), ncol = 7, dimnames = list(NULL, c("w", "mu", "beta", "delta", "Sigma", "misclustering_max", "misclustering_avg")))
error_mtlgmm <- rep(list(matrix(nrow = nrow(C_matrix), ncol = 7, dimnames = list(NULL, c("w", "mu", "beta", "delta", "Sigma", "misclustering_max", "misclustering_avg")))), 10)
error_pooling <- matrix(nrow = nrow(C_matrix), ncol = 7, dimnames = list(NULL, c("w", "mu", "beta", "delta", "Sigma", "misclustering_max", "misclustering_avg")))


for (i in 1:nrow(C_matrix)) {
  data_bundle <- data_generation(K = 10, outlier_K = C_matrix[i, 2], h = C_matrix[i, 1], n = 600, p = 15, simulation_no = "MTL-1")

  x_train <- sapply(1:10, function(k){
    data_bundle$data$x[[k]][1:100,]
  }, simplify = FALSE)
  x_test <- sapply(1:10, function(k){
    data_bundle$data$x[[k]][-(1:100),]
  }, simplify = FALSE)
  y_test <- sapply(1:10, function(k){
    data_bundle$data$y[[k]][-(1:100)]
  }, simplify = FALSE)


  # Single-task GMM
  x <- x_train
  fitted_values <- initialize(x, "EM")
  r <- alignment(fitted_values$beta, method = "exhaustive")
  fitted_values <- alignment_swap(r, initial_value_list = fitted_values)


  # MTL-GMM
  fit <- list()
  for (j in 1:10) {
    fit[[j]] <- mtlgmm(x = x, kappa = 1/3, initial_method = "EM", ncores = ncores, C_lambda = C_lambda_list[j],
                       lambda_choice = "fixed", step_size = "lipschitz", tol = 1e-3, trim = 0.1)
  }


  if (class(fit) != "try-error") {
    flag <- 0
  } else {
    next
  }

  # Pooling
  K <- 10
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


  if (C_matrix[i, "outlier_num"] == 0) {
    data_bundle$data$outlier_index <- 11
  }

  error_single[i, "w"] <- estimation_error(fitted_values$w[-data_bundle$data$outlier_index], data_bundle$parameter$w[-data_bundle$data$outlier_index], "w")
  error_pooling[i, "w"] <- estimation_error(fitted_values_pooled$w[-data_bundle$data$outlier_index], data_bundle$parameter$w[-data_bundle$data$outlier_index], "w")

  for (j in 1:10) {
    error_mtlgmm[[j]][i, "w"] <- estimation_error(fit[[j]]$w[-data_bundle$data$outlier_index], data_bundle$parameter$w[-data_bundle$data$outlier_index], "w")
    error_mtlgmm[[j]][i, "mu"] <- estimation_error(list(fit[[j]]$mu1[, -data_bundle$data$outlier_index], fit[[j]]$mu2[, -data_bundle$data$outlier_index]), list(data_bundle$parameter$mu1[, -data_bundle$data$outlier_index], data_bundle$parameter$mu2[, -data_bundle$data$outlier_index]), "mu")
    error_mtlgmm[[j]][i, "beta"] <- estimation_error(fit[[j]]$beta[, -data_bundle$data$outlier_index], data_bundle$parameter$beta[, -data_bundle$data$outlier_index], "beta")
    error_mtlgmm[[j]][i, "delta"] <- estimation_error(fit[[j]]$delta[-data_bundle$data$outlier_index], data_bundle$parameter$delta[-data_bundle$data$outlier_index], "delta")
    error_mtlgmm[[j]][i, "Sigma"] <- estimation_error(fit[[j]]$Sigma[-data_bundle$data$outlier_index], data_bundle$parameter$Sigma[-data_bundle$data$outlier_index], "Sigma")
    y_pred_mtlgmm <- sapply(1:10, function(k){
      predict_gmm(w = fit[[j]]$w[k], mu1 = fit[[j]]$mu1[, k], mu2 = fit[[j]]$mu2[, k], beta = fit[[j]]$beta[, k], newx = x_test[[k]])
    }, simplify = FALSE)
    error_mtlgmm[[j]][i, "misclustering_max"] <- misclustering_error(y_pred_mtlgmm[-data_bundle$data$outlier_index], y_test[-data_bundle$data$outlier_index], "max")
    error_mtlgmm[[j]][i, "misclustering_avg"] <- misclustering_error(y_pred_mtlgmm[-data_bundle$data$outlier_index], y_test[-data_bundle$data$outlier_index], "avg")
  }

  error_single[i, "mu"] <- estimation_error(list(fitted_values$mu1[, -data_bundle$data$outlier_index], fitted_values$mu2[, -data_bundle$data$outlier_index]), list(data_bundle$parameter$mu1[, -data_bundle$data$outlier_index], data_bundle$parameter$mu2[, -data_bundle$data$outlier_index]), "mu")
  error_pooling[i, "mu"] <- estimation_error(list(fitted_values_pooled$mu1[, -data_bundle$data$outlier_index], fitted_values_pooled$mu2[, -data_bundle$data$outlier_index]), list(data_bundle$parameter$mu1[, -data_bundle$data$outlier_index], data_bundle$parameter$mu2[, -data_bundle$data$outlier_index]), "mu")

  error_single[i, "beta"] <- estimation_error(fitted_values$beta[, -data_bundle$data$outlier_index], data_bundle$parameter$beta[, -data_bundle$data$outlier_index], "beta")
  error_pooling[i, "beta"] <- estimation_error(fitted_values_pooled$beta[, -data_bundle$data$outlier_index], data_bundle$parameter$beta[, -data_bundle$data$outlier_index], "beta")

  error_single[i, "delta"] <- estimation_error(fitted_values$delta[-data_bundle$data$outlier_index], data_bundle$parameter$delta[-data_bundle$data$outlier_index], "delta")
  error_pooling[i, "delta"] <- estimation_error(fitted_values_pooled$delta[-data_bundle$data$outlier_index], data_bundle$parameter$delta[-data_bundle$data$outlier_index], "delta")


  error_single[i, "Sigma"] <- estimation_error(fitted_values$Sigma[-data_bundle$data$outlier_index], data_bundle$parameter$Sigma[-data_bundle$data$outlier_index], "Sigma")
  error_pooling[i, "Sigma"] <- estimation_error(fitted_values_pooled$Sigma[-data_bundle$data$outlier_index], data_bundle$parameter$Sigma[-data_bundle$data$outlier_index], "Sigma")


  y_pred_single <- sapply(1:10, function(k){
    predict_gmm(w = fitted_values$w[k], mu1 = fitted_values$mu1[, k], mu2 = fitted_values$mu2[, k], beta = fitted_values$beta[, k], newx = x_test[[k]])
  }, simplify = FALSE)
  y_pred_pooling <- sapply(1:10, function(k){
    predict_gmm(w = fitted_values_pooled$w[k], mu1 = fitted_values_pooled$mu1[, k], mu2 = fitted_values_pooled$mu2[, k], beta = fitted_values_pooled$beta[, k], newx = x_test[[k]])
  }, simplify = FALSE)


  error_single[i, "misclustering_max"] <- misclustering_error(y_pred_single[-data_bundle$data$outlier_index], y_test[-data_bundle$data$outlier_index], "max")
  error_pooling[i, "misclustering_max"] <- misclustering_error(y_pred_pooling[-data_bundle$data$outlier_index], y_test[-data_bundle$data$outlier_index], "max")

  error_single[i, "misclustering_avg"] <- misclustering_error(y_pred_single[-data_bundle$data$outlier_index], y_test[-data_bundle$data$outlier_index], "avg")
  error_pooling[i, "misclustering_avg"] <- misclustering_error(y_pred_pooling[-data_bundle$data$outlier_index], y_test[-data_bundle$data$outlier_index], "avg")

}


save(error_single, error_pooling, error_mtlgmm, file = filename)




