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


filename = paste(relative_path, "/output/simulation/tl_1/", seed, ".RData", sep = "")
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
# Simulation of TL in the paper
# ---------------------------------------------------------
# See Section S.5.1.6 in the paper

h <- seq(0, 10, 1)
outlier_num <- 0:2
C_matrix <- as.matrix(expand.grid(h, outlier_num))
colnames(C_matrix) <- c("h", "outlier_num")


error_single <- matrix(nrow = nrow(C_matrix), ncol = 6, dimnames = list(NULL, c("w", "mu", "beta", "delta", "Sigma", "misclustering")))
error_mtlgmm_center <- matrix(nrow = nrow(C_matrix), ncol = 6, dimnames = list(NULL, c("w", "mu", "beta", "delta", "Sigma", "misclustering")))
error_mtlgmm <- matrix(nrow = nrow(C_matrix), ncol = 6, dimnames = list(NULL, c("w", "mu", "beta", "delta", "Sigma", "misclustering")))
error_pooling <- matrix(nrow = nrow(C_matrix), ncol = 6, dimnames = list(NULL, c("w", "mu", "beta", "delta", "Sigma", "misclustering")))
error_tlgmm <- matrix(nrow = nrow(C_matrix), ncol = 6, dimnames = list(NULL, c("w", "mu", "beta", "delta", "Sigma", "misclustering")))


for (i in 1:nrow(C_matrix)) {
  n.list <- rep(100, 11)
  data_bundle <- data_generation(K = 10, outlier_K = C_matrix[i, 2], h = C_matrix[i, 1], n = 600, p = 15, simulation_no = "MTL-1")

  data_bundle_target <- data_generation(K = 1, outlier_K = 0, n = 600, h = C_matrix[i, 1], p = 15, simulation_no = "MTL-1")

  x_train_mtl <- sapply(1:10, function(k){
    data_bundle$data$x[[k]]
  }, simplify = FALSE)
  x_train <- data_bundle_target$data$x[[1]][1:n.list[11],]
  x_test <- data_bundle_target$data$x[[1]][-(1:n.list[11]),]
  y_test <- data_bundle_target$data$y[[1]][-(1:n.list[11])]


  # Target-GMM
  fitted_values <- initialize(list(x_train), "EM")

  # MTL-GMM-center and TL-GMM
  fit <- mtlgmm(x = x_train_mtl, kappa = 1/3, initial_method = "EM", ncores = ncores, cv_nfolds = 10, cv_upper = "auto", cv_lower = 0.01, cv_length = 10,
                lambda_choice = "cv", step_size = "lipschitz", tol = 1e-3, trim = 0.1)

  fit.tl <- tlgmm(x = x_train, fitted_bar = fit, kappa0 = 1/3, initial_method = "EM", ncores = ncores, cv_length = 10,
                  cv_upper = "auto", cv_lower = 0.01, lambda_choice = "cv", step_size = "lipschitz")

  # MTL-GMM
  x_train_mtl[[11]] <- x_train
  fit_mtl <- mtlgmm(x = x_train_mtl, kappa = 1/3, initial_method = "EM", ncores = ncores, cv_nfolds = 10, cv_upper = "auto", cv_lower = 0.01, cv_length = 10,
                    lambda_choice = "cv", step_size = "lipschitz", tol = 1e-3, trim = 0.1)

  # Pooling
  K <- length(x_train_mtl)
  x.comb <- Reduce("rbind", x_train_mtl)
  fit_pooled <- Mclust(x.comb, G = 2, modelNames = "EEE")
  fitted_values_pooled <- list(w = NULL, mu1 = NULL, mu2 = NULL, beta = NULL, Sigma = NULL, delta = NULL)
  fitted_values_pooled$w <- fit_pooled$parameters$pro[2]
  fitted_values_pooled$mu1 <- fit_pooled$parameters$mean[,1]
  fitted_values_pooled$mu2 <- fit_pooled$parameters$mean[,2]
  fitted_values_pooled$Sigma <- fit_pooled$parameters$variance$Sigma
  fitted_values_pooled$beta <- solve(fit_pooled$parameters$variance$Sigma)%*% (fit_pooled$parameters$mean[,2] - fit_pooled$parameters$mean[,1])
  fitted_values_pooled$delta <- sum(fitted_values_pooled$beta*(fitted_values_pooled$mu1 + fitted_values_pooled$mu2)/2)


  error_single[i, "w"] <- estimation_error(fitted_values$w, data_bundle_target$parameter$w, "w")
  error_mtlgmm[i, "w"] <- estimation_error(fit_mtl$w[[11]], data_bundle_target$parameter$w, "w")
  error_pooling[i, "w"] <- estimation_error(fitted_values_pooled$w, data_bundle_target$parameter$w, "w")
  error_tlgmm[i, "w"] <- estimation_error(fit.tl$w, data_bundle_target$parameter$w, "w")

  error_single[i, "mu"] <- estimation_error(list(fitted_values$mu1, fitted_values$mu2), list(data_bundle_target$parameter$mu1, data_bundle_target$parameter$mu2), "mu")
  error_mtlgmm[i, "mu"] <- estimation_error(list(fit_mtl$mu1[, 11, drop = F], fit_mtl$mu2[, 11, drop = F]), list(data_bundle_target$parameter$mu1, data_bundle_target$parameter$mu2), "mu")
  error_pooling[i, "mu"] <- estimation_error(list(matrix(fitted_values_pooled$mu1), matrix(fitted_values_pooled$mu2)), list(data_bundle_target$parameter$mu1, data_bundle_target$parameter$mu2), "mu")
  error_tlgmm[i, "mu"] <- estimation_error(list(matrix(fit.tl$mu1), matrix(fit.tl$mu2)), list(data_bundle_target$parameter$mu1, data_bundle_target$parameter$mu2), "mu")

  error_single[i, "beta"] <- estimation_error(fitted_values$beta, data_bundle_target$parameter$beta, "beta")
  error_mtlgmm_center[i, "beta"] <- estimation_error(matrix(fit$beta_bar), data_bundle_target$parameter$beta, "beta")
  error_mtlgmm[i, "beta"] <- estimation_error(matrix(fit_mtl$beta[, 11]), data_bundle_target$parameter$beta, "beta")
  error_pooling[i, "beta"] <- estimation_error(fitted_values_pooled$beta, data_bundle_target$parameter$beta, "beta")
  error_tlgmm[i, "beta"] <- estimation_error(fit.tl$beta, data_bundle_target$parameter$beta, "beta")

  error_single[i, "Sigma"] <- estimation_error(fitted_values$Sigma, data_bundle_target$parameter$Sigma, "Sigma")
  error_mtlgmm[i, "Sigma"] <- estimation_error(fit_mtl$Sigma[11], data_bundle_target$parameter$Sigma, "Sigma")
  error_pooling[i, "Sigma"] <- estimation_error(list(fitted_values_pooled$Sigma), data_bundle_target$parameter$Sigma, "Sigma")
  error_tlgmm[i, "Sigma"] <- estimation_error(list(fit.tl$Sigma), data_bundle_target$parameter$Sigma, "Sigma")


  error_single[i, "delta"] <- estimation_error(fitted_values$delta, data_bundle_target$parameter$delta, "delta")
  error_pooling[i, "delta"] <- estimation_error(fitted_values_pooled$delta, data_bundle_target$parameter$delta, "delta")
  error_mtlgmm[i, "delta"] <- estimation_error(fit_mtl$delta[11], data_bundle_target$parameter$delta, "delta")
  error_tlgmm[i, "delta"] <- estimation_error(fit.tl$delta, data_bundle_target$parameter$delta, "delta")


  y_pred_single <- predict_gmm(w = fitted_values$w, mu1 = fitted_values$mu1, mu2 = fitted_values$mu2, beta = fitted_values$beta, newx = x_test)
  y_pred_mtlgmm <- predict_gmm(w = fit_mtl$w[11], mu1 = fit_mtl$mu1[, 11], mu2 = fit_mtl$mu2[, 11], beta = fit_mtl$beta[, 11], newx = x_test)
  y_pred_pooling <- predict_gmm(w = fitted_values_pooled$w, mu1 = fitted_values_pooled$mu1, mu2 = fitted_values_pooled$mu2, beta = fitted_values_pooled$beta, newx = x_test)
  y_pred_tlgmm <- predict_gmm(w = fit.tl$w, mu1 = fit.tl$mu1, mu2 = fit.tl$mu2, beta = fit.tl$beta, newx = x_test)


  error_single[i, "misclustering"] <- misclustering_error(y_pred_single, y_test, "max")
  error_mtlgmm[i, "misclustering"] <- misclustering_error(y_pred_mtlgmm, y_test, "max")
  error_pooling[i, "misclustering"] <- misclustering_error(y_pred_pooling, y_test, "max")
  error_tlgmm[i, "misclustering"] <- misclustering_error(y_pred_tlgmm, y_test, "max")

}


save(error_single, error_mtlgmm_center, error_mtlgmm, error_pooling, error_tlgmm, file = filename)




