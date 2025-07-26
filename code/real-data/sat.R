# Columbia Ginsburg

.libPaths("/burg/home/yt2661/R/x86_64-pc-linux-gnu-library/4.1/")


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
library(readxl)
library(here)

relative_path <- here()

conflict_prefer(name = "select", winner = "dplyr")
conflict_prefer(name = "filter", winner = "dplyr")

Sys.setenv(LANG = "en_US.UTF-8")
seed <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
cat("seed=", seed, "\n")

filename = paste(relative_path, "/output/real_data/sat/", seed, ".RData", sep = "")
if (file.exists(filename)) {
  stop("Done!")
}

set.seed(seed, kind = "L'Ecuyer-CMRG")

# -----------------------------------------------------------------------------------
beans <- read_xlsx(paste0(relative_path, "/data/beans/Dry_Bean_Dataset.xlsx"))


sat.train <- read.table(paste0(relative_path, "/data/sat/sat.trn"), header = FALSE)
sat.test <-  read.table(paste0(relative_path, "/data/sat/sat.tst"), header = FALSE)

sat <- rbind(sat.train, sat.test)

y <- sat$V37
y[y == 7] <- 6
y <- factor(y)
x <- sat[, -ncol(sat)]
test.ind <- Reduce("c", sapply(1:6, function(k){
  ind.k <- which(y == k)
  sample(ind.k, floor(length(ind.k)*0.1))
}, simplify = F))
x.train <- scale(x[-test.ind, ])
x.test <- scale(x[test.ind, ], scale = attr(x.train, "scaled:scale"), center = attr(x.train, "scaled:center"))
y.train <- y[-test.ind]
y.test <- y[test.ind]


error <- rep(list(matrix(nrow = 3, ncol = 6, dimnames = list(c("CX", "ER", "vanilla"), paste("class", 1:6)))), 4)
names(error) <- c("logistic", "knn", "svm", "randomforest")

w <- c(1, 1, 1, 1, 1, 1)/6
alpha <- c(NA, NA, 0.15, 0.2, 0.1, NA)


# logistic
fit.vanilla <- data.frame(x = x.train, y = y.train) %>% multinom(y~., data = ., trace = FALSE)
fit.npmc.CX <- try(npcs(x.train, y.train, algorithm = "CX", classifier = "logistic", w = w, alpha = alpha))
fit.npmc.ER <- try(npcs(x.train, y.train, algorithm = "ER", classifier = "logistic", w = w, alpha = alpha, refit = FALSE))

D.CX <- duality_check(fit.npmc.CX, x.train, y.train, delta = 0.1)
D.ER <- duality_check(fit.npmc.ER, x.train, y.train, delta = 0.2)

if (D.CX["f"] == 1) {
  y.pred.CX <- predict(fit.npmc.CX, x.test)
  error$logistic["CX", ] <- error_rate(y.pred.CX, y.test)
}

if (D.ER["f"] == 1) {
  y.pred.ER <- predict(fit.npmc.ER, x.test)
  error$logistic["ER", ] <- error_rate(y.pred.ER, y.test)
}

y.pred.vanilla <- data.frame(x = x.test) %>% predict(fit.vanilla,  newdata = .)
error$logistic["vanilla", ] <- error_rate(y.pred.vanilla, y.test)



# knn
fit.npmc.CX <- try(npcs(x.train, y.train, algorithm = "CX", classifier = "knn", w = w, alpha = alpha, k = floor(sqrt(length(y.train))/6)))
fit.npmc.ER <- try(npcs(x.train, y.train, algorithm = "ER", classifier = "knn", w = w, alpha = alpha, refit = FALSE, k = floor(sqrt(length(y.train))/6)))
fit.vanilla <- knn3(x = x.train, y = y.train, k = floor(sqrt(length(y.train))/6))

D.CX <- duality_check(fit.npmc.CX, x.train, y.train, delta = 0.1)
D.ER <- duality_check(fit.npmc.ER, x.train, y.train, delta = 0.2)

if (D.CX["f"] == 1) {
  y.pred.CX <- predict(fit.npmc.CX, x.test)
  error$knn["CX", ] <- error_rate(y.pred.CX, y.test)
}

if (D.ER["f"] == 1) {
  y.pred.ER <- predict(fit.npmc.ER, x.test)
  error$knn["ER", ] <- error_rate(y.pred.ER, y.test)
}

y.pred.vanilla <- predict(fit.vanilla, x.test, type = "class")
error$knn["vanilla", ] <- error_rate(y.pred.vanilla, y.test)

# RBF svm
fit.npmc.CX <- try(npcs(x.train, y.train, algorithm = "CX", classifier = "svm", w = w, alpha = alpha))
fit.npmc.ER <- try(npcs(x.train, y.train, algorithm = "ER", classifier = "svm", w = w, alpha = alpha, refit = FALSE))
fit.vanilla <- svm(x = x.train, y = y.train)

D.CX <- duality_check(fit.npmc.CX, x.train, y.train, delta = 0.1)
D.ER <- duality_check(fit.npmc.ER, x.train, y.train, delta = 0.2)

if (D.CX["f"] == 1) {
  y.pred.CX <- predict(fit.npmc.CX, x.test)
  error$svm["CX", ] <- error_rate(y.pred.CX, y.test)
}

if (D.ER["f"] == 1) {
  y.pred.ER <- predict(fit.npmc.ER, x.test)
  error$svm["ER", ] <- error_rate(y.pred.ER, y.test)
}

y.pred.vanilla <- predict(fit.vanilla, x.test)
error$svm["vanilla", ] <- error_rate(y.pred.vanilla, y.test)



# RF
fit.npmc.CX <- try(npcs(x.train, y.train, algorithm = "CX", classifier = "randomforest", w = w, alpha = alpha))
fit.npmc.ER <- try(npcs(x.train, y.train, algorithm = "ER", classifier = "randomforest", w = w, alpha = alpha, refit = FALSE))
fit.vanilla <- randomForest(x = x.train, y = y.train)

D.CX <- duality_check(fit.npmc.CX, x.train, y.train, delta = 0.1)
D.ER <- duality_check(fit.npmc.ER, x.train, y.train, delta = 0.2)

if (D.CX["f"] == 1) {
  y.pred.CX <- predict(fit.npmc.CX, x.test)
  error$randomforest["CX", ] <- error_rate(y.pred.CX, y.test)
}

if (D.ER["f"] == 1) {
  y.pred.ER <- predict(fit.npmc.ER, x.test)
  error$randomforest["ER", ] <- error_rate(y.pred.ER, y.test)
}

y.pred.vanilla <- predict(fit.vanilla, x.test)
error$randomforest["vanilla", ] <- error_rate(y.pred.vanilla, y.test)




save(error, file = filename)

