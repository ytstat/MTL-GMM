library(dplyr)
library(mclust)
library(doParallel)
library(MASS)
library(caret)
library(mtlgmm)
library(gtools)
library(DirichletReg)

relative_path <- here()

Sys.setenv(LANG = "en_US.UTF-8")
seed <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
cat("seed=", seed, "\n")


filename = paste(relative_path, "/output/simulation/mtl_diff_R/", seed, ".RData", sep = "")
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
# Simulation 6 of MTL in the paper, a variant of Simulation 2 but with varying numbers of clusters across tasks
# ---------------------------------------------------------
# See Section S.5.1.5 in the paper

h_list <- seq(0, 10, 1)
outlier_num <- 0:2
C_matrix <- as.matrix(expand.grid(h_list, outlier_num))
colnames(C_matrix) <- c("h", "outlier_num")

K <- 10
p <- 15

error_single <- matrix(nrow = nrow(C_matrix), ncol = 7, dimnames = list(NULL, c("w", "mu", "beta", "delta", "Sigma", "misclustering_max", "misclustering_avg")))
error_mtlgmm <- matrix(nrow = nrow(C_matrix), ncol = 7, dimnames = list(NULL, c("w", "mu", "beta", "delta", "Sigma", "misclustering_max", "misclustering_avg")))


for (i in 1:nrow(C_matrix)) {
  data_bundle <- data_generation(K = K, outlier_K = C_matrix[i, 2], h = C_matrix[i, 1], n = 600*2, p = p, simulation_no = "MTL-diff-R",
                                 prob = c(0.2, 0.3, 0.5))

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
  fitted_values <- initialize_multi(x_train, "EM", R = data_bundle$parameter$R, diff_R = TRUE)
  pihat <- alignment_multi(fitted_values$mu, fitted_values$Sigma, method = "greedy", num_replication = 50, ncores = ncores, diff_R = TRUE)
  fitted_values <- alignment_swap_multi(pihat, initial_value_list = fitted_values, diff_R = TRUE)

  R <- data_bundle$parameter$R
  R_max <- max(R)


  # MTL-GMM
  fit <- mtlgmm_multi_diff_R(x = x_train, kappa = 1/3, initial_method = "EM", ncores = ncores, cv_nfolds = 10, cv_upper = "auto", cv_length = 10,
                             lambda_choice = "cv", step_size = "lipschitz", tol = 1e-3, C_lambda = 5, R = R, alignment_method = "greedy", num_replication = 50)


  if (C_matrix[i, "outlier_num"] == 0) {
    data_bundle$data$outlier_index <- 11
  }

  error_single[i, "w"] <- estimation_error_multi(fitted_values$w, data_bundle$parameter$w, "w", outlier_index = data_bundle$data$outlier_index, diff_R = T)
  error_mtlgmm[i, "w"] <- estimation_error_multi(fit$w, data_bundle$parameter$w, "w", outlier_index = data_bundle$data$outlier_index, diff_R = T)

  error_single[i, "mu"] <- estimation_error_multi(fitted_values$mu, data_bundle$parameter$mu, "mu", outlier_index = data_bundle$data$outlier_index, diff_R = T)
  error_mtlgmm[i, "mu"] <- estimation_error_multi(fit$mu, data_bundle$parameter$mu, "mu", outlier_index = data_bundle$data$outlier_index, diff_R = T)

  error_single[i, "beta"] <- estimation_error_multi(fitted_values$beta, list(data_bundle$parameter$mu, data_bundle$parameter$Sigma), "beta", outlier_index = data_bundle$data$outlier_index, diff_R = T)
  error_mtlgmm[i, "beta"] <- estimation_error_multi(fit$beta, list(data_bundle$parameter$mu, data_bundle$parameter$Sigma), "beta", outlier_index = data_bundle$data$outlier_index, diff_R = T)

  error_single[i, "delta"] <- estimation_error_multi(fitted_values$delta, list(data_bundle$parameter$mu, data_bundle$parameter$Sigma), "delta", outlier_index = data_bundle$data$outlier_index, diff_R = T)
  error_mtlgmm[i, "delta"] <- estimation_error_multi(fit$delta, list(data_bundle$parameter$mu, data_bundle$parameter$Sigma), "delta", outlier_index = data_bundle$data$outlier_index, diff_R = T)

  error_single[i, "Sigma"] <- estimation_error_multi(fitted_values$Sigma, data_bundle$parameter$Sigma, "Sigma", outlier_index = data_bundle$data$outlier_index, diff_R = T)
  error_mtlgmm[i, "Sigma"] <- estimation_error_multi(fit$Sigma, data_bundle$parameter$Sigma, "Sigma", outlier_index = data_bundle$data$outlier_index, diff_R = T)


  y_pred_single <- sapply(1:K, function(k){
    mu_k <- sapply(1:R_max, function(r){
      fitted_values$mu[[r]][, k]
    })
    mu_k <- mu_k[, apply(mu_k, 2, function(x){!anyNA(x)})]
    beta_k <- sapply(1:ncol(mu_k), function(r){
      solve(fitted_values$Sigma[[k]]) %*% (mu_k[, r] - mu_k[, 1])
    })
    w_k <- fitted_values$w[!is.na(fitted_values$w[, k]), k]

    predict_gmm_multi(w = w_k, mu = mu_k, beta = beta_k, newx = x_test[[k]])
  }, simplify = FALSE)

  y_pred_mtlgmm <- sapply(1:K, function(k){
    mu_k <- sapply(1:R_max, function(r){
      fit$mu[[r]][, k]
    })
    mu_k <- mu_k[, apply(mu_k, 2, function(x){!anyNA(x)})]
    beta_k <- sapply(1:R_max, function(r){
      fit$beta[[r]][, k]
    })
    beta_k <- beta_k[, apply(beta_k, 2, function(x){!anyNA(x)})]
    w_k <- fit$w[!is.na(fit$w[, k]), k]
    predict_gmm_multi(w = w_k, mu = mu_k, beta = beta_k, newx = x_test[[k]])
  }, simplify = FALSE)

  error_single[i, "misclustering_max"] <- misclustering_error_multi(y_pred_single[-data_bundle$data$outlier_index], y_test[-data_bundle$data$outlier_index], R = R[-data_bundle$data$outlier_index], type = "max")
  error_mtlgmm[i, "misclustering_max"] <- misclustering_error_multi(y_pred_mtlgmm[-data_bundle$data$outlier_index], y_test[-data_bundle$data$outlier_index], R = R[-data_bundle$data$outlier_index], type = "max")

  error_single[i, "misclustering_avg"] <- misclustering_error_multi(y_pred_single[-data_bundle$data$outlier_index], y_test[-data_bundle$data$outlier_index], R = R[-data_bundle$data$outlier_index], type = "avg")
  error_mtlgmm[i, "misclustering_avg"] <- misclustering_error_multi(y_pred_mtlgmm[-data_bundle$data$outlier_index], y_test[-data_bundle$data$outlier_index], R = R[-data_bundle$data$outlier_index], type = "avg")

}


save(error_single, error_mtlgmm, file = filename)

