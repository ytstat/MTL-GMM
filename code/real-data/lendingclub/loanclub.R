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


filename = paste(relative_path, "/output/real_data/loanclub/loanclub/", seed, ".RData", sep = "")
if (file.exists(filename)) {
  stop("Done!")
}

set.seed(seed, kind = "L'Ecuyer-CMRG")


# -----------------------------------------------------------------------
# read data
# load("/Users/yetian/Library/CloudStorage/Dropbox/Columbia/Research/Project/Neyman-Pearson Multiclass Classification/datasets/loanclub/lc_loan_data_cleaned.RData")
load("/burg/home/yt2661/projects/NPMC/datasets/lc_loan_data_cleaned.RData")
x <- loan_data %>% select(-loan_status)
y <- loan_data$loan_status

# split data
split.ratio <- 0.5
ind.test <- Reduce("c", sapply(1:3, function(k){
  ind.k <- which(y == k)
  sample(ind.k, floor(length(ind.k)*split.ratio))
}, simplify = F))


x.train <- x[-ind.test,]
y.train <- y[-ind.test]
x.test <- x[ind.test,]
y.test <- y[ind.test]

# ---------------------------
# NPMC
# ---------------------------
error <- matrix(nrow = 4, ncol = 3, dimnames = list(c("CX-logistic", "logistic", "ER-randomforest", "randomforest"), c(paste("class", 1:3))))
w <- c(0, 0, 1)
alpha <- c(0.3, 0.5, NA)

# NPMC-ER: RF
fit.RF <- randomForest(x = x.train, y = factor(y.train))
y.pred.RF <- predict(fit.RF, x.test)
error["randomforest", ] <- error_rate(y.pred.RF, y.test)


fit.npmc.ER <- try(npcs(x.train, y.train, algorithm = "ER", classifier = "randomforest", w = w, alpha = alpha, refit = FALSE))
D <- duality_check(fit.npmc.ER, x.train, y.train, delta = 0.2, R.G = 10)

if (D["f"] == 1) {
  y.pred.ER <- predict(fit.npmc.ER, x.test)
  error["ER-randomforest", ] <- error_rate(y.pred.ER, y.test)
  print(error_rate(y.pred.ER, y.test))
}




# NPMC-CX: logistic
fit.npmc.CX <- try(npcs(x.train, y.train, algorithm = "CX", classifier = "logistic", w = w, alpha = alpha))
D <- duality_check(fit.npmc.CX, x.train, y.train, delta = 0.1, R.G = 10)

if (D["f"] == 1) {
  y.pred.CX <- predict(fit.npmc.CX, x.test)
  error["CX-logistic", ] <- error_rate(y.pred.CX, y.test)
  print(error_rate(y.pred.CX, y.test))
}

fit.logistic <- data.frame(x = x.train, y = y.train) %>% multinom(y~., data = ., maxit = 200, trace = FALSE)
y.pred.logistic <- predict(fit.logistic, newdata = data.frame(x = x.test))
error["logistic", ] <- error_rate(y.pred.logistic, y.test)


# ---------------------------
# GNPMC
# ---------------------------
w <- matrix(c(0, 0.1706, 0.1706,
              0.0729, 0, 0.0729,
              0.7565, 0.7565, 0), nrow = 3, byrow = TRUE)

alpha <- matrix(c(NA, NA, 0.1,
                  NA, NA, 0.15,
                  NA, NA, NA), nrow = 3, byrow = TRUE)

error_confusion <- matrix(nrow = 4, ncol = 3, dimnames = list(c("CX-logistic", "logistic", "ER-randomforest", "randomforest"), c("error13", "error23", "obj.value")))


# GNPMC-ER: RF
fit.npmc.ER <- try(npcs_confusion(x.train, y.train, algorithm = "ER", classifier = "randomforest", w = w, alpha = alpha, refit = FALSE))
D <- duality_check_confusion(fit.npmc.ER, x.train, y.train, delta = 0.2, R.G = 10)

if (D["f"] == 1) {
  y.pred.ER <- predict(fit.npmc.ER, x.test)
  error_confusion["ER-randomforest", 1:2] <- confusion_matrix(y.pred.ER, y.test)[1:2, 3]
  error_confusion["ER-randomforest", 3] <- sum(confusion_matrix(y.pred.ER, y.test)*w)
}


error_confusion["randomforest", 1:2] <- confusion_matrix(y.pred.RF, y.test)[1:2, 3]
error_confusion["randomforest", 3] <- sum(confusion_matrix(y.pred.RF, y.test)*w)


# GNPMC-CX: logistic
fit.npmc.CX <- try(npcs_confusion(x.train, y.train, algorithm = "CX", classifier = "logistic", w = w, alpha = alpha))
D <- duality_check_confusion(fit.npmc.CX, x.train, y.train, delta = 0.1, R.G = 10)

if (D["f"] == 1) {
  y.pred.CX <- predict(fit.npmc.CX, x.test)
  error_confusion["CX-logistic", 1:2] <- confusion_matrix(y.pred.CX, y.test)[1:2, 3]
  error_confusion["CX-logistic", 3] <- sum(confusion_matrix(y.pred.CX, y.test)*w)
}


error_confusion["logistic", 1:2] <- confusion_matrix(y.pred.logistic, y.test)[1:2, 3]
error_confusion["logistic", 3] <- sum(confusion_matrix(y.pred.logistic, y.test)*w)


save(error, error_confusion, file = filename)

