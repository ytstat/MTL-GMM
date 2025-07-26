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


filename = paste(relative_path, "/output/real_data/loanclub/loanclub_logistic/", seed, ".RData", sep = "")
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


# fit multinomial regression using all data
fit_multinomial <- data.frame(x = x, y = y) %>% multinom(y~., data = ., maxit = 200, trace = FALSE)


# NPMC
w <- c(0, 0, 1)
alpha1 <- seed/100
alpha <- c(alpha1, 0, NA)
D_matrix <- matrix(nrow = 100, ncol = 3, dimnames = list((1:100)/100, c("s", "f", "obj.value")))
for (i in 1:100) {
  print(i)
  alpha[2] <- i/100
  fit.npmc.CX <- try(npcs(x, y, algorithm = "CX", classifier = "logistic", w = w, alpha = alpha, limit = 1000,
                          fitted.model = fit_multinomial))
  D <- duality_check(fit.npmc.CX, x, y, delta = 0.1, R.G = 10)
  if (D["f"] == 1) {
    fit.npmc.CX$obj.value <- min(fit.npmc.CX$obj.value, 1+0.1)
  }
  D_matrix[i, 1:2] <- D
  D_matrix[i, 3] <- fit.npmc.CX$obj.value
}



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
  fit.npmc.CX <- try(npcs_confusion(x, y, algorithm = "CX", classifier = "logistic", w = w, alpha = alpha, limit = 1000,
                          fitted.model = fit_multinomial))
  D <- duality_check_confusion(fit.npmc.CX, x, y, delta = 0.1, R.G = 10)
  if (D["f"] == 1) {
    fit.npmc.CX$obj.value <- min(fit.npmc.CX$obj.value, 1+0.1)
  }
  D_matrix_confusion[i, 1:2] <- D
  D_matrix_confusion[i, 3] <- fit.npmc.CX$obj.value

  confusion_matrix(predict(fit.npmc.CX, x), y)
}


save(D_matrix, D_matrix_confusion, file = filename)

