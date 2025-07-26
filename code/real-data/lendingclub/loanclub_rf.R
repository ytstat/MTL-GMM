library(dfoptim)
library(nnet)
library(dplyr)
library(caret)
library(e1071)
library(randomForest)
library(npcs)
library(naivebayes)
library(MASS)
library(conflicted)
library(stringr)
library(here)

relative_path <- here()

conflict_prefer(name = "select", winner = "dplyr")
conflict_prefer(name = "filter", winner = "dplyr")

Sys.setenv(LANG = "en_US.UTF-8")
seed <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
cat("seed=", seed, "\n")


filename = paste(relative_path, "/output/real_data/loanclub/loanclub_rf/", seed, ".RData", sep = "")
if (file.exists(filename)) {
  stop("Done!")
}

set.seed(0, kind = "L'Ecuyer-CMRG")


# -----------------------------------------------------------------------
# read data
# load("/Users/yetian/Library/CloudStorage/Dropbox/Columbia/Research/Project/Neyman-Pearson Multiclass Classification/datasets/loanclub/lc_loan_data_cleaned.RData")
load("/burg/home/yt2661/projects/NPMC/datasets/lc_loan_data_cleaned.RData")
x <- loan_data %>% select(-loan_status)
y <- loan_data$loan_status

# # split data
# split.ratio <- 0.5
# ind <- Reduce("c", sapply(1:3, function(k){
#   ind.k <- which(y == k)
#   sample(ind.k, floor(length(ind.k)*split.ratio))
# }, simplify = F))
#
# # fit random forests using all data
# fit_rf <- randomForest(x = x[-ind, ], y = factor(y)[-ind])

# save(fit_rf, ind, file = "/Users/yetian/Library/CloudStorage/Dropbox/Columbia/Research/Project/Neyman-Pearson Multiclass Classification/codes/real-data/loanclub_detection_rf.RData")

load("/burg/home/yt2661/projects/NPMC/code/real_data/loanclub_rf.RData")

# NPMC
w <- c(0, 0, 1)
alpha1 <- seed/100
alpha <- c(alpha1, 0, NA)
D_matrix <- matrix(nrow = 100, ncol = 3, dimnames = list((1:100)/100, c("s", "f", "obj.value")))
for (i in 1:100) {
  print(i)
  alpha[2] <- i/100
  if(seed != 31 && seed != 17) {
    fit.npmc.ER <- try(npcs(x, y, algorithm = "ER", classifier = "randomforest", w = w, alpha = alpha, limit = 1000,
                            fitted.model = list(fit_rf, NA, ind), refit = FALSE))
  } else {
    fit.npmc.ER <- try(npcs(x, y, algorithm = "ER", classifier = "randomforest", w = w, alpha = alpha, limit = 500,
                            fitted.model = list(fit_rf, NA, ind), refit = FALSE))
  }

  D <- duality_check(fit.npmc.ER, x, y, delta = 0.2, R.G = 10)
  if (D["f"] == 1) {
    fit.npmc.ER$obj.value <- min(fit.npmc.ER$obj.value, 1+0.2)
  }
  D_matrix[i, 1:2] <- D
  D_matrix[i, 3] <- fit.npmc.ER$obj.value
}

# # ---
# D_matrix_confusion <- NA
# save(D_matrix, D_matrix_confusion, file = filename)
# # ---


# GNPMC
w <- matrix(c(0, 0.1706, 0.1706,
              0.0729, 0, 0.0729,
              0.7565, 0.7565, 0), nrow = 3, byrow = TRUE)
alpha13 <- seed/100
alpha <- matrix(c(NA, NA, alpha13,
                  NA, NA, 0,
                  NA, NA, NA), nrow = 3, byrow = TRUE)
D_matrix_confusion <- matrix(nrow = 100, ncol = 3, dimnames = list((1:100)/100, c("s", "f", "obj.value")))

for (i in 1:100) {
  print(i)
  alpha[2,3] <- i/100
  fit.npmc.ER <- try(npcs_confusion(x, y, algorithm = "ER", classifier = "randomforest", w = w, alpha = alpha, limit = 1000,
                          fitted.model = list(fit_rf, NA, ind), refit = FALSE))
  D <- duality_check_confusion(fit.npmc.ER, x, y, delta = 0.2, R.G = 10)
  if (D["f"] == 1) {
    fit.npmc.ER$obj.value <- min(fit.npmc.ER$obj.value, 1+0.2)
  }
  D_matrix_confusion[i, 1:2] <- D
  D_matrix_confusion[i, 3] <- fit.npmc.ER$obj.value
}


save(D_matrix, D_matrix_confusion, file = filename)

