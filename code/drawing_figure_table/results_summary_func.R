load_result <- function(folder_path, experiment_no = c("mtl_1", "mtl_2", "mtl_3", "mtl_1_epsilon", "mtl_epsilon",
                                                     "mtl_diff_R", "mtl_repre", "tl_1",
                                                     "mtl_1_tuning_C", "mtl_1_tuning_kappa",
                                                     "har", "har_multi", "pen", "pen_multi")) {

  experiment_no <- match.arg(experiment_no)

  if (experiment_no == "mtl_1") {
    h <- seq(0, 10, 1)
    outlier_num <- 0:2
    outlier_prop <- c(0, 0.1, 0.2)
    C_matrix <- as.matrix(expand.grid(h, outlier_num))
    colnames(C_matrix) <- c("h", "outlier_num")

    er.single <- er.pooling <- er.mtlgmm <- data.frame(C_matrix)

    er.single <- er.single %>% mutate(w_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), mu_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), beta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), delta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), Sigma_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_max_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_avg_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T))

    er.single <- er.single %>% mutate(w_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), mu_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), beta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), delta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), Sigma_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_max_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_avg_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T))


    # Pooling-GLM
    er.pooling <- er.pooling %>% mutate(w_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), mu_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), beta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), delta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), Sigma_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_max_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_avg_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T))

    er.pooling <- er.pooling %>% mutate(w_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), mu_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), beta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), delta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), Sigma_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_max_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_avg_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T))


    # MTL-GLM
    er.mtlgmm <- er.mtlgmm %>% mutate(w_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), mu_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), beta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), delta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), Sigma_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_max_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_avg_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T))

    er.mtlgmm <- er.mtlgmm %>% mutate(w_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), mu_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), beta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), delta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), Sigma_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_max_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_avg_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T))

    er.combined <- rbind(er.single, er.pooling, er.mtlgmm) %>% mutate(method = factor(rep(c("Single-task-GMM", "Pooled-GMM", "MTL-GMM"), each = nrow(er.single))))

    return(er.combined)
  } else if (experiment_no == "mtl_2") {
    h <- seq(0, 10, 1)
    outlier_num <- 0:2
    C_matrix <- as.matrix(expand.grid(h, outlier_num))
    colnames(C_matrix) <- c("h", "outlier_num")

    er.single <- er.pooling <- er.mtlgmm <- data.frame(C_matrix)

    er.single <- er.single %>% mutate(w_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), mu_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), beta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), delta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), Sigma_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_max_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_avg_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T))

    er.single <- er.single %>% mutate(w_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), mu_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), beta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), delta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), Sigma_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_max_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_avg_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T))


    # Pooling-GLM
    er.pooling <- er.pooling %>% mutate(w_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), mu_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), beta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), delta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), Sigma_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_max_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_avg_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T))

    er.pooling <- er.pooling %>% mutate(w_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), mu_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), beta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), delta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), Sigma_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_max_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_avg_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T))


    # MTL-GLM
    er.mtlgmm <- er.mtlgmm %>% mutate(w_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), mu_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), beta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), delta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), Sigma_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_max_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_avg_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T))

    er.mtlgmm <- er.mtlgmm %>% mutate(w_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), mu_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), beta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), delta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), Sigma_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_max_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_avg_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_2/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T))

    er.combined <- rbind(er.single, er.pooling, er.mtlgmm) %>% mutate(method = factor(rep(c("Single-task-GMM", "Pooled-GMM", "MTL-GMM"), each = nrow(er.single))))
    return(er.combined)

  } else if (experiment_no == "mtl_3") {
    h <- seq(0, 10, 1)
    outlier_num <- 0:2
    C_matrix <- as.matrix(expand.grid(h, outlier_num))
    colnames(C_matrix) <- c("h", "outlier_num")

    er.single <- er.pooling <- er.mtlgmm <- data.frame(C_matrix)

    er.single <- er.single %>% mutate(w_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), mu_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), beta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), delta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), Sigma_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_max_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_avg_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T))

    er.single <- er.single %>% mutate(w_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), mu_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), beta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), delta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), Sigma_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_max_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_avg_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T))


    # Pooling-GLM
    er.pooling <- er.pooling %>% mutate(w_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), mu_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), beta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), delta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), Sigma_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_max_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_avg_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T))

    er.pooling <- er.pooling %>% mutate(w_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), mu_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), beta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), delta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), Sigma_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_max_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_avg_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T))


    # MTL-GLM
    er.mtlgmm <- er.mtlgmm %>% mutate(w_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), mu_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), beta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), delta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), Sigma_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_max_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_avg_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T))

    er.mtlgmm <- er.mtlgmm %>% mutate(w_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), mu_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), beta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), delta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), Sigma_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_max_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_avg_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_3/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T))

    er.combined <- rbind(er.single, er.pooling, er.mtlgmm) %>% mutate(method = factor(rep(c("Single-task-GMM", "Pooled-GMM", "MTL-GMM"), each = nrow(er.single))))
    return(er.combined)
  } else if (experiment_no == "mtl_1_epsilon") {
    h <- seq(0, 10, 1)
    epsilon <- seq(0, 0.9, 0.1)
    C_matrix <- as.matrix(expand.grid(h, epsilon))
    colnames(C_matrix) <- c("h", "epsilon")

    er.single <- er.pooling <- er.mtlgmm <- data.frame(C_matrix)

    er.single <- er.single %>% mutate(w_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), mu_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), beta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), delta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), Sigma_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_max_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_avg_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T))

    er.single <- er.single %>% mutate(w_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), mu_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), beta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), delta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), Sigma_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_max_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_avg_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T))


    # Pooling-GLM
    er.pooling <- er.pooling %>% mutate(w_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), mu_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), beta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), delta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), Sigma_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_max_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_avg_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T))

    er.pooling <- er.pooling %>% mutate(w_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), mu_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), beta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), delta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), Sigma_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_max_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_avg_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T))


    # MTL-GLM
    er.mtlgmm <- er.mtlgmm %>% mutate(w_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), mu_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), beta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), delta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), Sigma_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_max_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_avg_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T))

    er.mtlgmm <- er.mtlgmm %>% mutate(w_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), mu_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), beta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), delta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), Sigma_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_max_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_avg_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T))

    er.combined <- rbind(er.single, er.pooling, er.mtlgmm) %>% filter(h == 0) %>% mutate(method = factor(rep(c("Single-task-GMM", "Pooled-GMM", "MTL-GMM"), each = 10)))
    return(er.combined)

  } else if (experiment_no == "mtl_epsilon") {
    h <- seq(0, 10, 1)
    epsilon <- seq(0, 0.9, 0.1)
    C_matrix <- as.matrix(expand.grid(h, epsilon))
    colnames(C_matrix) <- c("h", "epsilon")

    er.single <- er.pooling <- er.mtlgmm <- data.frame(C_matrix)

    er.single <- er.single %>% mutate(w_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), mu_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), beta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), delta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), Sigma_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_max_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_avg_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T))

    er.single <- er.single %>% mutate(w_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), mu_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), beta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), delta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), Sigma_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_max_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_avg_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T))


    # Pooling-GLM
    er.pooling <- er.pooling %>% mutate(w_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), mu_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), beta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), delta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), Sigma_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_max_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_avg_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T))

    er.pooling <- er.pooling %>% mutate(w_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), mu_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), beta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), delta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), Sigma_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_max_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_avg_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T))


    # MTL-GLM
    er.mtlgmm <- er.mtlgmm %>% mutate(w_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), mu_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), beta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), delta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), Sigma_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_max_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_avg_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T))

    er.mtlgmm <- er.mtlgmm %>% mutate(w_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), mu_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), beta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), delta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), Sigma_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_max_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_avg_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_epsilon/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T))

    er.combined <- rbind(er.single, er.pooling, er.mtlgmm) %>% filter(h == 0) %>% mutate(method = factor(rep(c("Single-task-GMM", "Pooled-GMM", "MTL-GMM"), each = 10)))
    return(er.combined)

  } else if (experiment_no == "mtl_diff_R") {
    h_list <- seq(0, 10, 1)
    outlier_num <- 0:2
    C_matrix <- as.matrix(expand.grid(h_list, outlier_num))
    colnames(C_matrix) <- c("h", "outlier_num")


    er.single <- er.mtlgmm <- data.frame(C_matrix)

    er.single <- er.single %>% mutate(w_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_diff_R/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), mu_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_diff_R/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), beta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_diff_R/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), delta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_diff_R/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), Sigma_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_diff_R/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_max_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_diff_R/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_avg_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_diff_R/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T))

    er.single <- er.single %>% mutate(w_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_diff_R/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), mu_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_diff_R/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), beta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_diff_R/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), delta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_diff_R/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), Sigma_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_diff_R/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_max_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_diff_R/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_avg_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_diff_R/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T))


    # MTL-GLM
    er.mtlgmm <- er.mtlgmm %>% mutate(w_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_diff_R/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), mu_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_diff_R/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), beta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_diff_R/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), delta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_diff_R/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), Sigma_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_diff_R/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_max_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_diff_R/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_avg_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_diff_R/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T))

    er.mtlgmm <- er.mtlgmm %>% mutate(w_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_diff_R/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), mu_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_diff_R/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), beta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_diff_R/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), delta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_diff_R/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), Sigma_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_diff_R/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_max_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_diff_R/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_avg_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_diff_R/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T))

    er.combined <- rbind(er.single, er.mtlgmm) %>% mutate(method = factor(rep(c("Single-task-GMM", "MTL-GMM"), each = nrow(er.single))))
    return(er.combined)

  } else if (experiment_no == "mtl_repre") {
    C_matrix <- matrix(c(20, 20, 3, 200,
                         20, 30, 5, 200,
                         30, 20, 3, 200,
                         30, 20, 5, 200,
                         30, 30, 5, 200,
                         50, 20, 5, 200,
                         50, 30, 5, 300,
                         100, 50, 5, 300,
                         100, 50, 10, 300), ncol = 4, byrow = TRUE, dimnames = list(NULL, c("K", "p", "d", "n")))

    er.single <- matrix(nrow = nrow(C_matrix), ncol = 2, dimnames = list(NULL, c("misclustering_max", "misclustering_avg")))
    er.mtlgmm <- matrix(nrow = nrow(C_matrix), ncol = 2, dimnames = list(NULL, c("misclustering_max", "misclustering_avg")))

    er.single <- matrix(rowMeans(sapply(1:200, function(i){
      load(paste0(folder_path,"/mtl_repre/",i,".RData"))
      as.vector(error_single)
    })), ncol = 2)

    er.single.sd <- matrix(apply(sapply(1:200, function(i){
      load(paste0(folder_path,"/mtl_repre/",i,".RData"))
      as.vector(error_single)
    }), 1, sd), ncol = 2)

    er.mtlgmm <- matrix(rowMeans(sapply(1:200, function(i){
      load(paste0(folder_path,"/mtl_repre/",i,".RData"))
      as.vector(error_mtlgmm)
    })), ncol = 2)

    er.mtlgmm.sd <- matrix(apply(sapply(1:200, function(i){
      load(paste0(folder_path,"/mtl_repre/",i,".RData"))
      as.vector(error_mtlgmm)
    }), 1, sd), ncol = 2)
    return(list(er.single, er.single.sd, er.mtlgmm, er.mtlgmm.sd))

  } else if (experiment_no == "misspecified_em") {
    nmi_mean <- nmi_sd <- data.frame(matrix(nrow = 88, ncol = 3))
    colnames(nmi_mean) <- colnames(nmi_sd) <- c("NMI", "method", "h")
    nmi_mean[, 2] <- nmi_sd[, 2] <- rep(c(paste0(folder_path,"/Single-task-GMM-", 2:5), paste0(folder_path,"/MTL-GMM-", 2:5)), 11)
    nmi_mean[, 3] <- nmi_sd[, 3] <- rep(0:10, each = 8)
    nmi_mean[, 1] <- rowMeans(sapply(1:200, function(i){
      load(paste0(folder_path,"/misspecified_em/",i,".RData"))
      as.numeric(nmi_df[, 1])
    }), na.rm = T)

    nmi_sd[, 1] <- apply(sapply(1:200, function(i){
      load(paste0(folder_path,"/misspecified_em/",i,".RData"))
      as.numeric(nmi_df[, 1])
    }), 1, function(x){sd(x, na.rm = T)})

    nmi_combined <- cbind(nmi_mean, nmi_sd[, 1])
    colnames(nmi_combined)[c(1, 4)] <- c("NMI_mean", "NMI_sd")
    return(nmi_combined)

  } else if (experiment_no == "tl_1") {

    h <- seq(0, 10, 1)
    outlier_num <- 0:2
    C_matrix <- as.matrix(expand.grid(h, outlier_num))
    colnames(C_matrix) <- c("h", "outlier_num")
    er.single <- er.pooling <- er.tlgmm <- er.mtlgmm <- er.mtlgmm_center <- data.frame(C_matrix)

    er.single <- er.single %>% mutate(w_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), mu_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), beta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), Sigma_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), delta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "misclustering"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T)) %>% mutate(w_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), mu_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), beta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), Sigma_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), delta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "misclustering"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T))


    # TL-GMM
    er.tlgmm <- er.tlgmm %>% mutate(w_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_tlgmm[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), mu_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_tlgmm[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), beta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_tlgmm[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), Sigma_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_tlgmm[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), delta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_tlgmm[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_tlgmm[, "misclustering"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T)) %>% mutate(w_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_tlgmm[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), mu_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_tlgmm[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), beta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_tlgmm[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), Sigma_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_tlgmm[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), delta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_tlgmm[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_tlgmm[, "misclustering"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T))


    # MTL-GMM
    er.mtlgmm <- er.mtlgmm %>% mutate(w_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), mu_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), beta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), Sigma_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), delta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "misclustering"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T)) %>% mutate(w_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), mu_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), beta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), delta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), Sigma_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm[, "misclustering"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T))


    # MTL-GMM-center
    er.mtlgmm_center <- er.mtlgmm_center %>% mutate(w_mean = rep(NA, 33), mu_mean =  rep(NA, 33), beta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm_center[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), Sigma_mean = rep(NA, 33), misclustering_mean =  rep(NA, 33)) %>% mutate(w_sd = rep(NA, 33), mu_sd =  rep(NA, 33), beta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_mtlgmm_center[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), delta_mean = rep(NA, 33), delta_sd = rep(NA, 33), Sigma_sd = rep(NA, 33), misclustering_sd =  rep(NA, 33))



    # Pooled-GMM
    er.pooling <- er.pooling %>% mutate(w_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), mu_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), beta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), Sigma_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), delta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "misclustering"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T)) %>% mutate(w_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), mu_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), beta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), Sigma_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), delta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/tl_1/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "misclustering"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T))

    er.combined <- rbind(er.single, er.mtlgmm, er.mtlgmm_center, er.pooling, er.tlgmm) %>% mutate(method = factor(rep(c("Target-GMM", "MTL-GMM", "MTL-GMM-center", "Pooled-GMM", "TL-GMM"), each = nrow(er.single))))
    return(er.combined)
  } else if (experiment_no == "mtl_1_tuning_C") {
    h <- seq(0, 10, 1)
    outlier_num <- 0:2
    C_matrix <- as.matrix(expand.grid(h, outlier_num))
    colnames(C_matrix) <- c("h", "outlier_num")

    er.single <- er.pooling <- data.frame(C_matrix)
    er.mtlgmm <- rep(list(data.frame(C_matrix)), 5)

    er.single <- er.single %>% mutate(w_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), mu_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), beta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), delta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), Sigma_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_max_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_avg_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T))

    er.single <- er.single %>% mutate(w_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), mu_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), beta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), delta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), Sigma_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_max_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_avg_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T))


    # Pooling-GLM
    er.pooling <- er.pooling %>% mutate(w_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), mu_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), beta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), delta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), Sigma_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_max_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_avg_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T))

    er.pooling <- er.pooling %>% mutate(w_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), mu_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), beta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), delta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), Sigma_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_max_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_avg_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T))


    # MTL-GLM
    for (j in 1:5) {
      er.mtlgmm[[j]] <- er.mtlgmm[[j]] %>% mutate(w_mean = apply(sapply(1:200, function(i){
        a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
        if (class(a) != "try-error") {
          error_mtlgmm[[5+j]][, "w"]
        } else {
          rep(NA, 66)
        }
      }), 1, mean, na.rm = T), mu_mean =  apply(sapply(1:200, function(i){
        a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
        if (class(a) != "try-error") {
          error_mtlgmm[[5+j]][, "mu"]
        } else {
          rep(NA, 66)
        }
      }), 1, mean, na.rm = T), beta_mean = apply(sapply(1:200, function(i){
        a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
        if (class(a) != "try-error") {
          error_mtlgmm[[5+j]][, "beta"]
        } else {
          rep(NA, 66)
        }
      }), 1, mean, na.rm = T), delta_mean = apply(sapply(1:200, function(i){
        a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
        if (class(a) != "try-error") {
          error_mtlgmm[[5+j]][, "delta"]
        } else {
          rep(NA, 66)
        }
      }), 1, mean, na.rm = T), Sigma_mean = apply(sapply(1:200, function(i){
        a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
        if (class(a) != "try-error") {
          error_mtlgmm[[5+j]][, "Sigma"]
        } else {
          rep(NA, 66)
        }
      }), 1, mean, na.rm = T), misclustering_max_mean =  apply(sapply(1:200, function(i){
        a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
        if (class(a) != "try-error") {
          error_mtlgmm[[5+j]][, "misclustering_max"]
        } else {
          rep(NA, 66)
        }
      }), 1, mean, na.rm = T), misclustering_avg_mean =  apply(sapply(1:200, function(i){
        a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
        if (class(a) != "try-error") {
          error_mtlgmm[[5+j]][, "misclustering_avg"]
        } else {
          rep(NA, 66)
        }
      }), 1, mean, na.rm = T))

      er.mtlgmm[[j]] <- er.mtlgmm[[j]] %>% mutate(w_sd = apply(sapply(1:200, function(i){
        a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
        if (class(a) != "try-error") {
          error_mtlgmm[[5+j]][, "w"]
        } else {
          rep(NA, 66)
        }
      }), 1, sd, na.rm = T), mu_sd =  apply(sapply(1:200, function(i){
        a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
        if (class(a) != "try-error") {
          error_mtlgmm[[5+j]][, "mu"]
        } else {
          rep(NA, 66)
        }
      }), 1, sd, na.rm = T), beta_sd = apply(sapply(1:200, function(i){
        a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
        if (class(a) != "try-error") {
          error_mtlgmm[[5+j]][, "beta"]
        } else {
          rep(NA, 66)
        }
      }), 1, sd, na.rm = T), delta_sd = apply(sapply(1:200, function(i){
        a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
        if (class(a) != "try-error") {
          error_mtlgmm[[5+j]][, "delta"]
        } else {
          rep(NA, 66)
        }
      }), 1, sd, na.rm = T), Sigma_sd = apply(sapply(1:200, function(i){
        a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
        if (class(a) != "try-error") {
          error_mtlgmm[[5+j]][, "Sigma"]
        } else {
          rep(NA, 66)
        }
      }), 1, sd, na.rm = T), misclustering_max_sd =  apply(sapply(1:200, function(i){
        a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
        if (class(a) != "try-error") {
          error_mtlgmm[[5+j]][, "misclustering_max"]
        } else {
          rep(NA, 66)
        }
      }), 1, sd, na.rm = T), misclustering_avg_sd =  apply(sapply(1:200, function(i){
        a <- try(load(paste0(folder_path,"/mtl_1_tuning_C/",i,".RData")))
        if (class(a) != "try-error") {
          error_mtlgmm[[5+j]][, "misclustering_avg"]
        } else {
          rep(NA, 66)
        }
      }), 1, sd, na.rm = T))
    }

    C_lambda_list <- exp(seq(log(0.1), log(10), length.out = 10))
    er.combined <- rbind(er.single, er.pooling, Reduce("rbind", er.mtlgmm)) %>% mutate(method = factor(rep(c("Single-task-GMM", "Pooled-GMM", paste0(folder_path,"/MTL-GMM-", round(C_lambda_list[seq(6,10,1)], digits = 2))), each = nrow(er.single)), levels = c(paste0(folder_path,"/MTL-GMM-", round(C_lambda_list[seq(6,10,1)], digits = 2)), "Single-task-GMM", "Pooled-GMM")))

    return(er.combined)
  } else if (experiment_no == "mtl_1_tuning_kappa") {
    h <- seq(0, 10, 1)
    outlier_num <- 0:2
    C_matrix <- as.matrix(expand.grid(h, outlier_num))
    colnames(C_matrix) <- c("h", "outlier_num")

    er.single <- er.pooling <- data.frame(C_matrix)
    er.mtlgmm <- rep(list(data.frame(C_matrix)), 5)

    er.single <- er.single %>% mutate(w_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), mu_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), beta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), delta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), Sigma_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_max_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_avg_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T))

    er.single <- er.single %>% mutate(w_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), mu_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), beta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), delta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), Sigma_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_max_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_avg_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
      if (class(a) != "try-error") {
        error_single[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T))


    # Pooling-GLM
    er.pooling <- er.pooling %>% mutate(w_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), mu_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), beta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), delta_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), Sigma_mean = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_max_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T), misclustering_avg_mean =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, mean, na.rm = T))

    er.pooling <- er.pooling %>% mutate(w_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "w"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), mu_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "mu"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), beta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "beta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), delta_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "delta"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), Sigma_sd = apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "Sigma"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_max_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "misclustering_max"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T), misclustering_avg_sd =  apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
      if (class(a) != "try-error") {
        error_pooling[, "misclustering_avg"]
      } else {
        rep(NA, 66)
      }
    }), 1, sd, na.rm = T))


    # MTL-GLM
    for (j in 1:5) {
      er.mtlgmm[[j]] <- er.mtlgmm[[j]] %>% mutate(w_mean = apply(sapply(1:200, function(i){
        a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
        if (class(a) != "try-error") {
          error_mtlgmm[[2*(j-1)+1]][, "w"]
        } else {
          rep(NA, 66)
        }
      }), 1, mean, na.rm = T), mu_mean =  apply(sapply(1:200, function(i){
        a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
        if (class(a) != "try-error") {
          error_mtlgmm[[2*(j-1)+1]][, "mu"]
        } else {
          rep(NA, 66)
        }
      }), 1, mean, na.rm = T), beta_mean = apply(sapply(1:200, function(i){
        a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
        if (class(a) != "try-error") {
          error_mtlgmm[[2*(j-1)+1]][, "beta"]
        } else {
          rep(NA, 66)
        }
      }), 1, mean, na.rm = T), delta_mean = apply(sapply(1:200, function(i){
        a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
        if (class(a) != "try-error") {
          error_mtlgmm[[2*(j-1)+1]][, "delta"]
        } else {
          rep(NA, 66)
        }
      }), 1, mean, na.rm = T), Sigma_mean = apply(sapply(1:200, function(i){
        a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
        if (class(a) != "try-error") {
          error_mtlgmm[[2*(j-1)+1]][, "Sigma"]
        } else {
          rep(NA, 66)
        }
      }), 1, mean, na.rm = T), misclustering_max_mean =  apply(sapply(1:200, function(i){
        a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
        if (class(a) != "try-error") {
          error_mtlgmm[[2*(j-1)+1]][, "misclustering_max"]
        } else {
          rep(NA, 66)
        }
      }), 1, mean, na.rm = T), misclustering_avg_mean =  apply(sapply(1:200, function(i){
        a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
        if (class(a) != "try-error") {
          error_mtlgmm[[2*(j-1)+1]][, "misclustering_avg"]
        } else {
          rep(NA, 66)
        }
      }), 1, mean, na.rm = T))

      er.mtlgmm[[j]] <- er.mtlgmm[[j]] %>% mutate(w_sd = apply(sapply(1:200, function(i){
        a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
        if (class(a) != "try-error") {
          error_mtlgmm[[2*(j-1)+1]][, "w"]
        } else {
          rep(NA, 66)
        }
      }), 1, sd, na.rm = T), mu_sd =  apply(sapply(1:200, function(i){
        a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
        if (class(a) != "try-error") {
          error_mtlgmm[[2*(j-1)+1]][, "mu"]
        } else {
          rep(NA, 66)
        }
      }), 1, sd, na.rm = T), beta_sd = apply(sapply(1:200, function(i){
        a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
        if (class(a) != "try-error") {
          error_mtlgmm[[2*(j-1)+1]][, "beta"]
        } else {
          rep(NA, 66)
        }
      }), 1, sd, na.rm = T), delta_sd = apply(sapply(1:200, function(i){
        a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
        if (class(a) != "try-error") {
          error_mtlgmm[[2*(j-1)+1]][, "delta"]
        } else {
          rep(NA, 66)
        }
      }), 1, sd, na.rm = T), Sigma_sd = apply(sapply(1:200, function(i){
        a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
        if (class(a) != "try-error") {
          error_mtlgmm[[2*(j-1)+1]][, "Sigma"]
        } else {
          rep(NA, 66)
        }
      }), 1, sd, na.rm = T), misclustering_max_sd =  apply(sapply(1:200, function(i){
        a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
        if (class(a) != "try-error") {
          error_mtlgmm[[2*(j-1)+1]][, "misclustering_max"]
        } else {
          rep(NA, 66)
        }
      }), 1, sd, na.rm = T), misclustering_avg_sd =  apply(sapply(1:200, function(i){
        a <- try(load(paste0(folder_path,"/mtl_1_tuning_kappa/",i,".RData")))
        if (class(a) != "try-error") {
          error_mtlgmm[[2*(j-1)+1]][, "misclustering_avg"]
        } else {
          rep(NA, 66)
        }
      }), 1, sd, na.rm = T))
    }

    kappa_list <- seq(0.1, 0.9, 0.1)
    er.combined <- rbind(er.single, er.pooling, Reduce("rbind", er.mtlgmm)) %>% mutate(method = factor(rep(c("Single-task-GMM", "Pooled-GMM", paste0(folder_path,"/MTL-GMM-", kappa_list[seq(1,9,2)])), each = nrow(er.single)), levels = c(paste0(folder_path,"/MTL-GMM-", kappa_list[seq(1,9,2)]), "Single-task-GMM", "Pooled-GMM")))
    return(er.combined)

  } else if (experiment_no == "har") {
    # table
    er.har <- rowMeans(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/har/",i,".RData")))
      if (class(a) != "try-error") {
        error
      } else {
        rep(NA, 6)
      }
    }), na.rm = TRUE)


    er.har.sd <- apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/har/",i,".RData")))
      if (class(a) != "try-error") {
        error
      } else {
        rep(NA, 6)
      }
    }), 1, sd)

    names(er.har) <- c("Single-task-GMM max", "MTL-GMM max", "Pooled-GMM max", "Single-task-GMM avg", "MTL-GMM avg", "Pooled-GMM avg")
    names(er.har.sd) <- c("Single-task-GMM max", "MTL-GMM max", "Pooled-GMM max", "Single-task-GMM avg", "MTL-GMM avg", "Pooled-GMM avg")

    # plot
    er.har.single <- rowMeans(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/har/",i,".RData")))
      if (class(a) != "try-error") {
        error_all[1, ]
      } else {
        rep(NA, 30)
      }
    }), na.rm = TRUE)

    er.har.mtlgmm <- rowMeans(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/har/",i,".RData")))
      if (class(a) != "try-error") {
        error_all[2, ]
      } else {
        rep(NA, 30)
      }
    }), na.rm = TRUE)

    er.har.pooling <- rowMeans(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/har/",i,".RData")))
      if (class(a) != "try-error") {
        error_all[3, ]
      } else {
        rep(NA, 30)
      }
    }), na.rm = TRUE)

    er.har.single.sd <- apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/har/",i,".RData")))
      if (class(a) != "try-error") {
        error_all[1, ]
      } else {
        rep(NA, 30)
      }
    }), 1, sd)

    er.har.mtlgmm.sd <- apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/har/",i,".RData")))
      if (class(a) != "try-error") {
        error_all[2, ]
      } else {
        rep(NA, 30)
      }
    }), 1, sd)

    er.har.pooling.sd <- apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/har/",i,".RData")))
      if (class(a) != "try-error") {
        error_all[3, ]
      } else {
        rep(NA, 30)
      }
    }), 1, sd)



    er.har.all <- data.frame(task = rep(1:30, 3), method = factor(rep(c("Single-task-GMM", "Pooled-GMM", "MTL-GMM"), each = 30)), error =
                               c(er.har.single, er.har.pooling, er.har.mtlgmm), error_sd = c(er.har.single.sd, er.har.pooling.sd, er.har.mtlgmm.sd))

    return(list(er.har, er.har.sd, er.har.all))
  } else if (experiment_no == "har_multi") {
    # table
    er.har <- rowMeans(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/har_multi/",i,".RData")))
      if (class(a) != "try-error") {
        error
      } else {
        rep(NA, 6)
      }
    }), na.rm = TRUE)


    er.har.sd <- apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/har_multi/",i,".RData")))
      if (class(a) != "try-error") {
        error
      } else {
        rep(NA, 6)
      }
    }), 1, sd)

    names(er.har) <- c("Single-task-GMM max", "MTL-GMM max", "Pooled-GMM max", "Single-task-GMM avg", "MTL-GMM avg", "Pooled-GMM avg")
    names(er.har.sd) <- c("Single-task-GMM max", "MTL-GMM max", "Pooled-GMM max", "Single-task-GMM avg", "MTL-GMM avg", "Pooled-GMM avg")
    er.har
    er.har.sd

    # plot
    er.har.single <- rowMeans(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/har_multi/",i,".RData")))
      if (class(a) != "try-error") {
        error_all[1, ]
      } else {
        rep(NA, 30)
      }
    }), na.rm = TRUE)

    er.har.mtlgmm <- rowMeans(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/har_multi/",i,".RData")))
      if (class(a) != "try-error") {
        error_all[2, ]
      } else {
        rep(NA, 30)
      }
    }), na.rm = TRUE)

    er.har.pooling <- rowMeans(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/har_multi/",i,".RData")))
      if (class(a) != "try-error") {
        error_all[3, ]
      } else {
        rep(NA, 30)
      }
    }), na.rm = TRUE)

    er.har.single.sd <- apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/har_multi/",i,".RData")))
      if (class(a) != "try-error") {
        error_all[1, ]
      } else {
        rep(NA, 30)
      }
    }), 1, sd)

    er.har.mtlgmm.sd <- apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/har_multi/",i,".RData")))
      if (class(a) != "try-error") {
        error_all[2, ]
      } else {
        rep(NA, 30)
      }
    }), 1, sd)

    er.har.pooling.sd <- apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/har_multi/",i,".RData")))
      if (class(a) != "try-error") {
        error_all[3, ]
      } else {
        rep(NA, 30)
      }
    }), 1, sd)



    er.har.all <- data.frame(task = rep(1:30, 3), method = factor(rep(c("Single-task-GMM", "Pooled-GMM", "MTL-GMM"), each = 30)), error =
                               c(er.har.single, er.har.pooling, er.har.mtlgmm), error_sd = c(er.har.single.sd, er.har.pooling.sd, er.har.mtlgmm.sd))

    return(list(er.har, er.har.sd, er.har.all))
  } else if (experiment_no == "pen") {
    er.pen <- rowMeans(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/pen/",i,".RData")))
      if (class(a) != "try-error") {
        error
      } else {
        rep(NA, 6)
      }
    }), na.rm = TRUE)


    er.pen.sd <- apply(sapply(1:100, function(i){
      a <- try(load(paste0(folder_path,"/pen/",i,".RData")))
      if (class(a) != "try-error") {
        error
      } else {
        rep(NA, 6)
      }
    }), 1, sd)


    names(er.pen) <- c("Single-task-GMM max", "MTL-GMM max", "Pooled-GMM max", "Single-task-GMM avg", "MTL-GMM avg", "Pooled-GMM avg")
    names(er.pen.sd) <- c("Single-task-GMM max", "MTL-GMM max", "Pooled-GMM max", "Single-task-GMM avg", "MTL-GMM avg", "Pooled-GMM avg")

    # plot
    er.pen.single <- rowMeans(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/pen/",i,".RData")))
      if (class(a) != "try-error") {
        error_all[1, ]
      } else {
        rep(NA, 30)
      }
    }), na.rm = TRUE)

    er.pen.mtlgmm <- rowMeans(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/pen/",i,".RData")))
      if (class(a) != "try-error") {
        error_all[2, ]
      } else {
        rep(NA, 30)
      }
    }), na.rm = TRUE)

    er.pen.pooling <- rowMeans(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/pen/",i,".RData")))
      if (class(a) != "try-error") {
        error_all[3, ]
      } else {
        rep(NA, 30)
      }
    }), na.rm = TRUE)

    er.pen.single.sd <- apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/pen/",i,".RData")))
      if (class(a) != "try-error") {
        error_all[1, ]
      } else {
        rep(NA, 30)
      }
    }), 1, sd)

    er.pen.mtlgmm.sd <- apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/pen/",i,".RData")))
      if (class(a) != "try-error") {
        error_all[2, ]
      } else {
        rep(NA, 30)
      }
    }), 1, sd)

    er.pen.pooling.sd <- apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/pen/",i,".RData")))
      if (class(a) != "try-error") {
        error_all[3, ]
      } else {
        rep(NA, 30)
      }
    }), 1, sd)


    er.pen.all <- data.frame(task = rep(1:44, 3), method = factor(rep(c("Single-task-GMM", "Pooled-GMM", "MTL-GMM"), each = 44)), error =
                               c(er.pen.single, er.pen.pooling, er.pen.mtlgmm), error_sd = c(er.pen.single.sd, er.pen.pooling.sd, er.pen.mtlgmm.sd))

    return(list(er.pen, er.pen.sd, er.pen.all))
  } else if (experiment_no == "pen_multi") {
    # table
    er.pen <- rowMeans(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/pen_multi/",i,".RData")))
      if (class(a) != "try-error") {
        error
      } else {
        rep(NA, 6)
      }
    }), na.rm = TRUE)


    er.pen.sd <- apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/pen_multi/",i,".RData")))
      if (class(a) != "try-error") {
        error
      } else {
        rep(NA, 6)
      }
    }), 1, sd)


    names(er.pen) <- c("Single-task-GMM max", "MTL-GMM max", "Pooled-GMM max", "Single-task-GMM avg", "MTL-GMM avg", "Pooled-GMM avg")
    names(er.pen.sd) <- c("Single-task-GMM max", "MTL-GMM max", "Pooled-GMM max", "Single-task-GMM avg", "MTL-GMM avg", "Pooled-GMM avg")

    # plot
    er.pen.single <- rowMeans(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/pen_multi/",i,".RData")))
      if (class(a) != "try-error") {
        error_all[1, ]
      } else {
        rep(NA, 30)
      }
    }), na.rm = TRUE)

    er.pen.mtlgmm <- rowMeans(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/pen_multi/",i,".RData")))
      if (class(a) != "try-error") {
        error_all[2, ]
      } else {
        rep(NA, 30)
      }
    }), na.rm = TRUE)

    er.pen.pooling <- rowMeans(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/pen_multi/",i,".RData")))
      if (class(a) != "try-error") {
        error_all[3, ]
      } else {
        rep(NA, 30)
      }
    }), na.rm = TRUE)

    er.pen.single.sd <- apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/pen_multi/",i,".RData")))
      if (class(a) != "try-error") {
        error_all[1, ]
      } else {
        rep(NA, 30)
      }
    }), 1, sd)

    er.pen.mtlgmm.sd <- apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/pen_multi/",i,".RData")))
      if (class(a) != "try-error") {
        error_all[2, ]
      } else {
        rep(NA, 30)
      }
    }), 1, sd)

    er.pen.pooling.sd <- apply(sapply(1:200, function(i){
      a <- try(load(paste0(folder_path,"/pen_multi/",i,".RData")))
      if (class(a) != "try-error") {
        error_all[3, ]
      } else {
        rep(NA, 30)
      }
    }), 1, sd)


    er.pen.all <- data.frame(task = rep(1:44, 3), method = factor(rep(c("Single-task-GMM", "Pooled-GMM", "MTL-GMM"), each = 44)), error =
                               c(er.pen.single, er.pen.pooling, er.pen.mtlgmm), error_sd = c(er.pen.single.sd, er.pen.pooling.sd, er.pen.mtlgmm.sd))

    return(list(er.pen, er.pen.sd, er.pen.all))
  }


}
