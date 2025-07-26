library(dplyr)
library(mclust)
library(doParallel)
library(MASS)
library(caret)
library(mtlgmm)
library(gtools)
library(DirichletReg)
library(here)

relative_path <- here()

Sys.setenv(LANG = "en_US.UTF-8")
seed <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
cat("seed=", seed, "\n")


filename = paste(relative_path, "/output/simulation/mtl_2/", seed, ".RData", sep = "")
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
# Simulation 2 of MTL in the paper
# ---------------------------------------------------------
# See Section S.5.1.2 in the paper

h_list <- seq(0, 10, 1)
outlier_num <- 0:2
C_matrix <- as.matrix(expand.grid(h_list, outlier_num))
colnames(C_matrix) <- c("h", "outlier_num")

K <- 10
R <- 4
p <- 15

error_single <- matrix(nrow = nrow(C_matrix), ncol = 7, dimnames = list(NULL, c("w", "mu", "beta", "delta", "Sigma", "misclustering_max", "misclustering_avg")))
error_mtlgmm <- matrix(nrow = nrow(C_matrix), ncol = 7, dimnames = list(NULL, c("w", "mu", "beta", "delta", "Sigma", "misclustering_max", "misclustering_avg")))
error_pooling <- matrix(nrow = nrow(C_matrix), ncol = 7, dimnames = list(NULL, c("w", "mu", "beta", "delta", "Sigma", "misclustering_max", "misclustering_avg")))


for (i in 1:nrow(C_matrix)) {
  data_bundle <- data_generation(K = K, outlier_K = C_matrix[i, 2], h = C_matrix[i, 1], n = 600*2, p = 15, simulation_no = "MTL-2")

  x_train <- sapply(1:K, function(k){
    data_bundle$data$x[[k]][1:200,]
  }, simplify = FALSE)

  x_test <- sapply(1:K, function(k){
    data_bundle$data$x[[k]][-(1:200),]
  }, simplify = FALSE)
  y_test <- sapply(1:K, function(k){
    data_bundle$data$y[[k]][-(1:200)]
  }, simplify = FALSE)


  # Single-task GMM
  x <- x_train
  fitted_values <- initialize_multi(x, "EM")
  pihat <- alignment_multi(fitted_values$mu, fitted_values$Sigma, method = "greedy", num_replication = 50, ncores = ncores)
  fitted_values <- alignment_swap_multi(pihat, initial_value_list = fitted_values)


  # MTL-GMM
  fit <- mtlgmm_multi(x = x, kappa = 1/3, initial_method = "EM", ncores = ncores, cv_nfolds = 10, cv_upper = "auto", cv_length = 10,
                      lambda_choice = "cv", step_size = "lipschitz", tol = 1e-3, C_lambda = 5, R = 4, alignment_method = "greedy",
                      num_replication = 50)


  # Pooled-GMM
  x.comb <- Reduce("rbind", x)
  fit_pooled <- Mclust(x.comb, G = 4, modelNames = "EEE")
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


  if (C_matrix[i, "outlier_num"] == 0) {
    data_bundle$data$outlier_index <- 11
  }

  error_single[i, "w"] <- estimation_error_multi(fitted_values$w, data_bundle$parameter$w, "w", outlier_index = data_bundle$data$outlier_index)
  error_pooling[i, "w"] <- estimation_error_multi(fitted_values_pooled$w, data_bundle$parameter$w, "w", outlier_index = data_bundle$data$outlier_index)
  error_mtlgmm[i, "w"] <- estimation_error_multi(fit$w, data_bundle$parameter$w, "w", outlier_index = data_bundle$data$outlier_index)

  error_single[i, "mu"] <- estimation_error_multi(fitted_values$mu, data_bundle$parameter$mu, "mu", outlier_index = data_bundle$data$outlier_index)
  error_pooling[i, "mu"] <- estimation_error_multi(fitted_values_pooled$mu, data_bundle$parameter$mu, "mu", outlier_index = data_bundle$data$outlier_index)
  error_mtlgmm[i, "mu"] <- estimation_error_multi(fit$mu, data_bundle$parameter$mu, "mu", outlier_index = data_bundle$data$outlier_index)

  error_single[i, "beta"] <- estimation_error_multi(fitted_values$beta, list(data_bundle$parameter$mu, data_bundle$parameter$Sigma), "beta", outlier_index = data_bundle$data$outlier_index)
  error_pooling[i, "beta"] <- estimation_error_multi(fitted_values_pooled$beta, list(data_bundle$parameter$mu, data_bundle$parameter$Sigma), "beta", outlier_index = data_bundle$data$outlier_index)
  error_mtlgmm[i, "beta"] <- estimation_error_multi(fit$beta, list(data_bundle$parameter$mu, data_bundle$parameter$Sigma), "beta", outlier_index = data_bundle$data$outlier_index)

  error_single[i, "delta"] <- estimation_error_multi(fitted_values$delta, list(data_bundle$parameter$mu, data_bundle$parameter$Sigma), "delta", outlier_index = data_bundle$data$outlier_index)
  error_pooling[i, "delta"] <- estimation_error_multi(fitted_values_pooled$delta, list(data_bundle$parameter$mu, data_bundle$parameter$Sigma), "delta", outlier_index = data_bundle$data$outlier_index)
  error_mtlgmm[i, "delta"] <- estimation_error_multi(fit$delta, list(data_bundle$parameter$mu, data_bundle$parameter$Sigma), "delta", outlier_index = data_bundle$data$outlier_index)


  error_single[i, "Sigma"] <- estimation_error_multi(fitted_values$Sigma, data_bundle$parameter$Sigma, "Sigma", outlier_index = data_bundle$data$outlier_index)
  error_pooling[i, "Sigma"] <- estimation_error_multi(fitted_values_pooled$Sigma, data_bundle$parameter$Sigma, "Sigma", outlier_index = data_bundle$data$outlier_index)
  error_mtlgmm[i, "Sigma"] <- estimation_error_multi(fit$Sigma, data_bundle$parameter$Sigma, "Sigma", outlier_index = data_bundle$data$outlier_index)


  y_pred_single <- sapply(1:K, function(k){
    mu_k <- sapply(1:R, function(r){
      fitted_values$mu[[r]][, k]
    })
    beta_k <- sapply(1:R, function(r){
      solve(fitted_values$Sigma[[k]]) %*% (mu_k[, r] - mu_k[, 1])
    })
    predict_gmm_multi(w = fitted_values$w[, k], mu = mu_k, beta = beta_k, newx = x_test[[k]])
  }, simplify = FALSE)
  y_pred_pooling <- sapply(1:K, function(k){
    mu_k <- sapply(1:R, function(r){
      fitted_values_pooled$mu[[r]][, k]
    })
    beta_k <- sapply(1:R, function(r){
      solve(fitted_values_pooled$Sigma[[k]]) %*% (mu_k[, r] - mu_k[, 1])
    })
    predict_gmm_multi(w = fitted_values_pooled$w[, k], mu = mu_k, beta = beta_k, newx = x_test[[k]])
  }, simplify = FALSE)
  y_pred_mtlgmm <- sapply(1:K, function(k){
    mu_k <- sapply(1:R, function(r){
      fit$mu[[r]][, k]
    })
    beta_k <- sapply(1:R, function(r){
      fit$beta[[r]][, k]
    })
    predict_gmm_multi(w = fit$w[, k], mu = mu_k, beta = beta_k, newx = x_test[[k]])
  }, simplify = FALSE)

  error_single[i, "misclustering_max"] <- misclustering_error_multi(y_pred_single[-data_bundle$data$outlier_index], y_test[-data_bundle$data$outlier_index], R = 4, type = "max")
  error_pooling[i, "misclustering_max"] <- misclustering_error_multi(y_pred_pooling[-data_bundle$data$outlier_index], y_test[-data_bundle$data$outlier_index], R = 4, type = "max")
  error_mtlgmm[i, "misclustering_max"] <- misclustering_error_multi(y_pred_mtlgmm[-data_bundle$data$outlier_index], y_test[-data_bundle$data$outlier_index], R = 4, type = "max")

  error_single[i, "misclustering_avg"] <- misclustering_error_multi(y_pred_single[-data_bundle$data$outlier_index], y_test[-data_bundle$data$outlier_index], R = 4, type = "avg")
  error_pooling[i, "misclustering_avg"] <- misclustering_error_multi(y_pred_pooling[-data_bundle$data$outlier_index], y_test[-data_bundle$data$outlier_index], R = 4, type = "avg")
  error_mtlgmm[i, "misclustering_avg"] <- misclustering_error_multi(y_pred_mtlgmm[-data_bundle$data$outlier_index], y_test[-data_bundle$data$outlier_index], R = 4, type = "avg")

}


save(error_single, error_pooling, error_mtlgmm, file = filename)




