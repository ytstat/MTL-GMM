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


filename = paste(relative_path, "/output/simulation/mtl_repre/", seed, ".RData", sep = "")
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
# Simulation 7 of MTL in the paper, designed for an extension to the case of different cluster numbers across tasks, based on a shared low-rank structure
# ---------------------------------------------------------
# See Section S.5.1.5 in the paper

C_matrix <- matrix(c(20, 20, 3, 200,
                     20, 30, 5, 200,
                     30, 20, 3, 200,
                     30, 20, 5, 200,
                     30, 30, 5, 200,
                     50, 20, 5, 200,
                     50, 30, 5, 300,
                     100, 50, 5, 300,
                     100, 50, 10, 300), ncol = 4, byrow = TRUE, dimnames = list(NULL, c("K", "p", "d", "n")))

error_single <- matrix(nrow = nrow(C_matrix), ncol = 2, dimnames = list(NULL, c("misclustering_max", "misclustering_avg")))
error_mtlgmm <- matrix(nrow = nrow(C_matrix), ncol = 2, dimnames = list(NULL, c("misclustering_max", "misclustering_avg")))


for (i in 1:nrow(C_matrix)) {
  K <- C_matrix[i, "K"]
  p <- C_matrix[i, "p"]
  d <- C_matrix[i, "d"]
  n_train <- C_matrix[i, "n"]
  n_test <- 1000

  data_bundle <- data_generation(K = K, outlier_K = 0, h = 0, n = n_train + n_test, p = p, simulation_no = "MTL-repre", d = d)

  x_train <- sapply(1:K, function(k){
    data_bundle$data$x[[k]][1:n_train,]
  }, simplify = FALSE)

  x_test <- sapply(1:K, function(k){
    data_bundle$data$x[[k]][-(1:n_train),]
  }, simplify = FALSE)
  y_test <- sapply(1:K, function(k){
    data_bundle$data$y[[k]][-(1:n_train)]
  }, simplify = FALSE)

  R <- data_bundle$parameter$R


  # Single-task GMM
  x <- x_train
  fit_mcluster <- sapply(1:K, function(k){
    Mclust(x[[k]], G = R[k], modelNames = "EEE")
  }, simplify = FALSE)
  w_0 <- sapply(1:K, function(k){
    sapply(1:R[k], function(r){
      fit_mcluster[[k]]$parameters$pro[r]
    })
  }, simplify = FALSE)

  mu_0 <- sapply(1:K, function(k){
    sapply(1:R[k], function(r){
      fit_mcluster[[k]]$parameters$mean[, r]
    })
  }, simplify = FALSE)

  Sigma_0 <- sapply(1:K, function(k){
    fit_mcluster[[k]]$parameters$variance$Sigma
  }, simplify = FALSE)

  beta_0 <- sapply(1:K, function(k){
    sapply(1:R[k], function(r){
      solve(Sigma_0[[k]]) %*% (mu_0[[k]][, r] - mu_0[[k]][, 1])
    })
  }, simplify = FALSE)

  # learn the representation
  A_mu <- svd(Reduce(cbind, mu_0))$u[,1:d]

  # refit
  fit_mcluster_A <- sapply(1:K, function(k){
    Mclust(x[[k]]%*%A_mu, G = R[k], modelNames = "EEE")
  }, simplify = FALSE)

  w_A <- sapply(1:K, function(k){
    sapply(1:R[k], function(r){
      fit_mcluster_A[[k]]$parameters$pro[r]
    })
  }, simplify = FALSE)

  mu_A <- sapply(1:K, function(k){
    sapply(1:R[k], function(r){
      fit_mcluster_A[[k]]$parameters$mean[, r]
    })
  }, simplify = FALSE)



  Sigma_A <- sapply(1:K, function(k){
    fit_mcluster_A[[k]]$parameters$variance$Sigma
  }, simplify = FALSE)


  beta_A <- sapply(1:K, function(k){
    sapply(1:R[k], function(r){
      solve(Sigma_A[[k]]) %*% (mu_A[[k]][, r] - mu_A[[k]][, 1])
    })
  }, simplify = FALSE)


  delta_A <- sapply(1:K, function(k){
    sapply(1:R[k], function(r){
      sum(beta_A[[k]][, r]*(mu_A[[k]][, r] + mu_A[[k]][, 1])/2)
    })
  }, simplify = F)


  y_pred_single <- sapply(1:K, function(k){
    predict_gmm_multi(w = w_0[[k]], mu = mu_0[[k]], beta = beta_0[[k]], newx = x_test[[k]])
  }, simplify = FALSE)
  y_pred_A <- sapply(1:K, function(k){
    predict_gmm_multi(w = w_A[[k]], mu = mu_A[[k]], beta = beta_A[[k]], newx = x_test[[k]]%*%A_mu)
  }, simplify = FALSE)

  error_single[i, "misclustering_max"] <- misclustering_error_multi(y_pred_single, y_test, R = R, type = "max")
  error_mtlgmm[i, "misclustering_max"] <- misclustering_error_multi(y_pred_A, y_test, R = R, type = "max")

  error_single[i, "misclustering_avg"] <- misclustering_error_multi(y_pred_single, y_test, R = R, type = "avg")
  error_mtlgmm[i, "misclustering_avg"] <- misclustering_error_multi(y_pred_A, y_test, R = R, type = "avg")


}


save(error_single, error_mtlgmm, file = filename)




