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
library(here)

relative_path <- here()

conflict_prefer(name = "select", winner = "dplyr")
conflict_prefer(name = "filter", winner = "dplyr")

Sys.setenv(LANG = "en_US.UTF-8")
seed <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
cat("seed=", seed, "\n")

filename = paste(relative_path, "/output/real_data/dementia/", seed, ".RData", sep = "")
if (file.exists(filename)) {
  stop("Done!")
}

set.seed(seed, kind = "L'Ecuyer-CMRG")

# ----------------------------------------------------------
data <- read.csv(paste0(relative_path, "/data/dementia/dementia.csv"))
cols = c(8,9,13,19,20,21,22,23,24,25,26,27,29,32)
data = data[,cols]

data = data[complete.cases(data),]

allx = data[,-ncol(data)]
ally = data[, ncol(data)]
allx$sex <- as.factor(allx$sex)
allx$race <- as.factor(allx$race)
rownames(allx) <- NULL

ally[ally == "NL"] <- 1
ally[ally == "AD"] <- 2
ally[!(ally %in% 1:2)] <- 3
ally <- factor(as.numeric(ally))

N <- nrow(allx)

ind1 <- which(ally==1)
ind2 <- which(ally==2)
ind3 <- which(ally==3)
frac = 0.8

tr_size1 = floor(length(ind1)*frac)
tr_size2 = floor(length(ind2)*frac)
tr_size3 = floor(length(ind3)*frac)



alpha <- c(0.1, 0.02, NA)
w <- c(0, 0, 1)



error <- rep(list(matrix(nrow = 3, ncol = 3, dimnames = list(c("CX", "ER", "vanilla"), paste("class", 1:3)))), 4)
names(error) <- c("logistic", "knn", "svm", "randomforest")
error.smote <- rep(list(matrix(nrow = 3, ncol = 3, dimnames = list(c("CX", "ER", "vanilla"), paste("class", 1:3)))), 4)
names(error.smote) <- c("logistic", "knn", "svm", "randomforest")


# sampling
train.ind1 <- sample(ind1, tr_size1)
train.ind2 <- sample(ind2, tr_size2)
train.ind3 <- sample(ind3, tr_size3)

allx.numeric <- model.matrix(~.-1,allx)

x.train <- allx.numeric[c(train.ind1, train.ind2, train.ind3), ]
y.train <- ally[c(train.ind1, train.ind2, train.ind3)]

x.scale.info <- scale(x.train[, !(colnames(x.train) %in% c("sex1", "sex2", "race2", "race5", "race50"))])
x.train[, !(colnames(x.train) %in% c("sex1", "sex2", "race2", "race5", "race50"))] <- x.scale.info

x.test <- allx.numeric[-c(train.ind1, train.ind2, train.ind3), ]
x.test[, !(colnames(x.test) %in% c("sex1", "sex2", "race2", "race5", "race50"))] <- scale(x.test[, !(colnames(x.test) %in% c("sex1", "sex2", "race2", "race5", "race50"))], scale = attr(x.scale.info, "scaled:scale"), center = attr(x.scale.info, "scaled:center"))

y.test <- ally[-c(train.ind1, train.ind2, train.ind3)]

rownames(x.train) <- NULL


# -------------------------------------
# without SMOTE
# -------------------------------------
# logistic
fit.vanilla <- data.frame(x = x.train, y = y.train) %>% multinom(y~., data = ., trace = FALSE)
fit.npmc.CX <- try(npcs(x.train, y.train, algorithm = "CX", classifier = "logistic", w = w, alpha = alpha))
fit.npmc.ER <- try(npcs(x.train, y.train, algorithm = "ER", classifier = "logistic", w = w, alpha = alpha, refit = FALSE, split.ratio = 0.5))

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
fit.vanilla <- knn3(x = x.train, y = y.train, k = floor(sqrt(length(y.train))/3))
fit.npmc.CX <- try(npcs(x.train, y.train, algorithm = "CX", classifier = "knn", w = w, alpha = alpha, k = floor(sqrt(length(y.train))/3)))
fit.npmc.ER <- try(npcs(x.train, y.train, algorithm = "ER", classifier = "knn", w = w, alpha = alpha, refit = FALSE, split.ratio = 0.5, k = floor(sqrt(length(y.train))/3)))

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




# RF
fit.vanilla <- randomForest(x = x.train, y = y.train)
fit.npmc.CX <- try(npcs(x.train, y.train, algorithm = "CX", classifier = "randomforest", w = w, alpha = alpha))
fit.npmc.ER <- try(npcs(x.train, y.train, algorithm = "ER", classifier = "randomforest", w = w, alpha = alpha, refit = FALSE, split.ratio = 0.5))

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


# SVM
fit.vanilla <- svm(x = x.train, y = y.train)
fit.npmc.CX <- try(npcs(x.train, y.train, algorithm = "CX", classifier = "svm", w = w, alpha = alpha))
fit.npmc.ER <- try(npcs(x.train, y.train, algorithm = "ER", classifier = "svm", w = w, alpha = alpha, refit = FALSE, split.ratio = 0.5))

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


# -------------------------------------
# with SMOTE
# -------------------------------------
D.syn <- gamma_smote(x.train, y.train, dup_rate = 4, gamma = 0.5, k = 5)
x.train <- D.syn$x
y.train <- D.syn$y

# logistic
fit.vanilla <- data.frame(x = x.train, y = y.train) %>% multinom(y~., data = ., trace = FALSE)
fit.npmc.CX <- try(npcs(x.train, y.train, algorithm = "CX", classifier = "logistic", w = w, alpha = alpha))
fit.npmc.ER <- try(npcs(x.train, y.train, algorithm = "ER", classifier = "logistic", w = w, alpha = alpha, refit = T, split.ratio = 0.5))

D.CX <- duality_check(fit.npmc.CX, x.train, y.train, delta = 0.1)
D.ER <- duality_check(fit.npmc.ER, x.train, y.train, delta = 0.2)


if (D.CX["f"] == 1) {
  y.pred.CX <- predict(fit.npmc.CX, x.test)
  error.smote$logistic["CX", ] <- error_rate(y.pred.CX, y.test)
}

if (D.ER["f"] == 1) {
  y.pred.ER <- predict(fit.npmc.ER, x.test)
  error.smote$logistic["ER", ] <- error_rate(y.pred.ER, y.test)
}


y.pred.vanilla <- data.frame(x = x.test) %>% predict(fit.vanilla,  newdata = .)
error.smote$logistic["vanilla", ] <- error_rate(y.pred.vanilla, y.test)




# knn
fit.vanilla <- knn3(x = x.train, y = y.train, k = floor(sqrt(length(y.train))/3))
fit.npmc.CX <- try(npcs(x.train, y.train, algorithm = "CX", classifier = "knn", w = w, alpha = alpha, k = floor(sqrt(length(y.train))/3)))
fit.npmc.ER <- try(npcs(x.train, y.train, algorithm = "ER", classifier = "knn", w = w, alpha = alpha, refit = FALSE, split.ratio = 0.5, k = floor(sqrt(length(y.train))/3)))

D.CX <- duality_check(fit.npmc.CX, x.train, y.train, delta = 0.1)
D.ER <- duality_check(fit.npmc.ER, x.train, y.train, delta = 0.2)


if (D.CX["f"] == 1) {
  y.pred.CX <- predict(fit.npmc.CX, x.test)
  error.smote$knn["CX", ] <- error_rate(y.pred.CX, y.test)
}

if (D.ER["f"] == 1) {
  y.pred.ER <- predict(fit.npmc.ER, x.test)
  error.smote$knn["ER", ] <- error_rate(y.pred.ER, y.test)
}


y.pred.vanilla <- predict(fit.vanilla, x.test, type = "class")
error.smote$knn["vanilla", ] <- error_rate(y.pred.vanilla, y.test)




# RF
fit.vanilla <- randomForest(x = x.train, y = y.train)
fit.npmc.CX <- try(npcs(x.train, y.train, algorithm = "CX", classifier = "randomforest", w = w, alpha = alpha))
fit.npmc.ER <- try(npcs(x.train, y.train, algorithm = "ER", classifier = "randomforest", w = w, alpha = alpha, refit = FALSE, split.ratio = 0.5))

D.CX <- duality_check(fit.npmc.CX, x.train, y.train, delta = 0.1)
D.ER <- duality_check(fit.npmc.ER, x.train, y.train, delta = 0.2)


if (D.CX["f"] == 1) {
  y.pred.CX <- predict(fit.npmc.CX, x.test)
  error.smote$randomforest["CX", ] <- error_rate(y.pred.CX, y.test)
}

if (D.ER["f"] == 1) {
  y.pred.ER <- predict(fit.npmc.ER, x.test)
  error.smote$randomforest["ER", ] <- error_rate(y.pred.ER, y.test)
}


y.pred.vanilla <- predict(fit.vanilla, x.test)
error.smote$randomforest["vanilla", ] <- error_rate(y.pred.vanilla, y.test)


# SVM
fit.vanilla <- svm(x = x.train, y = y.train)
fit.npmc.CX <- try(npcs(x.train, y.train, algorithm = "CX", classifier = "svm", w = w, alpha = alpha))
fit.npmc.ER <- try(npcs(x.train, y.train, algorithm = "ER", classifier = "svm", w = w, alpha = alpha, refit = FALSE, split.ratio = 0.5))

D.CX <- duality_check(fit.npmc.CX, x.train, y.train, delta = 0.1)
D.ER <- duality_check(fit.npmc.ER, x.train, y.train, delta = 0.2)


if (D.CX["f"] == 1) {
  y.pred.CX <- predict(fit.npmc.CX, x.test)
  error.smote$svm["CX", ] <- error_rate(y.pred.CX, y.test)
}

if (D.ER["f"] == 1) {
  y.pred.ER <- predict(fit.npmc.ER, x.test)
  error.smote$svm["ER", ] <- error_rate(y.pred.ER, y.test)
}

y.pred.vanilla <- predict(fit.vanilla, x.test)
error.smote$svm["vanilla", ] <- error_rate(y.pred.vanilla, y.test)



save(error, error.smote, file = filename)
