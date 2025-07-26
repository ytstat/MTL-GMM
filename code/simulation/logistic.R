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

filename = paste(relative_path, "/output/simulation/logistic/", seed, ".RData", sep = "")
if (file.exists(filename)) {
  stop("Done!")
}

set.seed(seed, kind = "L'Ecuyer-CMRG")


# ---------------------------------------------------------
# model 1: "case 1" in the paper
# ---------------------------------------------------------
n.list <- seq(1000, 10000, 1000)
alpha <- c(0.15, 0.3, NA)
w <- c(0, 0, 1)
error1 <- matrix(nrow = 4, ncol = 3, dimnames = list(c("CX", "ER", "vanilla", "MNPO"), c("class 1", "class 2", "class 3")))
error1 <- rep(list(error1), 10)
time.list1 <- matrix(nrow = length(n.list), ncol = 4, dimnames = list(n.list, c("CX", "ER", "vanilla", "MNPO")))
for (i in 1:length(n.list)) {
  print(i)
  train.set <- generate_data(n=n.list[i], model.no = 3)
  x <- train.set$x
  y <- train.set$y

  test.set <- generate_data(n=2e4, model.no = 3)
  x.test <- test.set$x
  y.test <- test.set$y

  t1.CX <- proc.time()
  fit.npmc.CX <- try(npcs(x, y, algorithm = "CX", classifier = "logistic", w = w, alpha = alpha))
  D.CX <- duality_check(fit.npmc.CX, x, y, delta = 0.1)
  if (D.CX["f"] == 1) {
    y.pred.CX <- predict(fit.npmc.CX, x.test)
  }
  t2.CX <- proc.time()
  time.list1[i, "CX"] <- (t2.CX - t1.CX)["elapsed"]
  if (D.CX["f"] == 1) {
    error1[[i]]["CX", ] <- error_rate(y.pred.CX, y.test)
  } else {
    error1[[i]]["CX", ] <- rep(NA, 3)
  }


  t1.ER <- proc.time()
  fit.npmc.ER <- try(npcs(x, y, algorithm = "ER", classifier = "logistic", w = w, alpha = alpha, refit = FALSE))
  D.ER <- duality_check(fit.npmc.ER, x, y, delta = 0.2)
  if (D.ER["f"] == 1) {
    y.pred.ER <- predict(fit.npmc.ER, x.test)
  }
  t2.ER <- proc.time()
  time.list1[i, "ER"] <- (t2.ER - t1.ER)["elapsed"]
  if (D.ER["f"] == 1) {
    error1[[i]]["ER", ] <- error_rate(y.pred.ER, y.test)
  } else {
    error1[[i]]["ER", ] <- rep(NA, 3)
  }


  t1.vanilla <- proc.time()
  fit.vanilla <- data.frame(x = x, y = factor(y)) %>% multinom(y~., data = ., trace = FALSE)
  y.pred.vanilla <- data.frame(x = x.test) %>% predict(fit.vanilla,  newdata = .)
  t2.vanilla <- proc.time()
  time.list1[i, "vanilla"] <- (t2.vanilla - t1.vanilla)["elapsed"]
  error1[[i]]["vanilla", ] <- error_rate(y.pred.vanilla, y.test)


  t1.mnpo <- proc.time()
  fit.mnpo <- mnpo(x, y, classifier = "logistic", alpha = alpha, w = w, increment = 0.01)
  if (is.list(fit.mnpo)) {
    y.pred.mnpo <- predict_mnpo(fit.mnpo, x.test)
  }
  t2.mnpo <- proc.time()
  time.list1[i, "MNPO"] <- (t2.mnpo - t1.mnpo)["elapsed"]
  if (is.list(fit.mnpo)) {
    error1[[i]]["MNPO", ] <- error_rate(y.pred.mnpo, y.test)
  } else {
    error1[[i]]["MNPO", ] <- rep(NA, 3)
  }


}


# ---------------------------------------------------------
# model 2: "case 2" in the paper
# ---------------------------------------------------------
n.list <- seq(1000, 10000, 1000)
alpha <- c(0.04, NA, 0.08, NA)
w <- c(0.1, 0.2, 0.3, 0.4)
error2 <- matrix(nrow = 3, ncol = 5, dimnames = list(c("CX", "ER", "vanilla"), c("class 1", "class 2", "class 3", "class 4", "obj.value")))
error2 <- rep(list(error2), 10)

for (i in 1:length(n.list)) {
  print(i)
  train.set <- generate_data(n=n.list[i], model.no = 2)
  x <- train.set$x
  y <- train.set$y

  test.set <- generate_data(n=2e4, model.no = 2)
  x.test <- test.set$x
  y.test <- test.set$y

  fit.npmc.CX <- try(npcs(x, y, algorithm = "CX", classifier = "logistic", w = w, alpha = alpha))
  fit.npmc.ER <- try(npcs(x, y, algorithm = "ER", classifier = "logistic", w = w, alpha = alpha, refit = FALSE))
  fit.vanilla <- data.frame(x = x, y = factor(y)) %>% multinom(y~., data = ., trace = FALSE)

  D.CX <- duality_check(fit.npmc.CX, x, y, delta = 0.1)
  D.ER <- duality_check(fit.npmc.ER, x, y, delta = 0.2)

  if (D.CX["f"] == 1) {
    y.pred.CX <- predict(fit.npmc.CX, x.test)
    error2[[i]]["CX", 1:4] <- error_rate(y.pred.CX, y.test)
    error2[[i]]["CX", 5] <- sum(w*error_rate(y.pred.CX, y.test))
  } else {
    error2[[i]]["CX", ] <- rep(NA, 5)
  }


  if (D.ER["f"] == 1) {
    y.pred.ER <- predict(fit.npmc.ER, x.test)
    error2[[i]]["ER", 1:4] <- error_rate(y.pred.ER, y.test)
    error2[[i]]["ER", 5] <- sum(w*error_rate(y.pred.ER, y.test))
  } else {
    error2[[i]]["ER", ] <- rep(NA, 5)
  }

  y.pred.vanilla <- data.frame(x = x.test) %>% predict(fit.vanilla,  newdata = .)
  error2[[i]]["vanilla", 1:4] <- error_rate(y.pred.vanilla, y.test)
  error2[[i]]["vanilla", 5] <- sum(w*error_rate(y.pred.vanilla, y.test))
}


# ---------------------------------------------------------
# model 3: "case 4" in the paper
# ---------------------------------------------------------
n.list <- seq(1000, 10000, 1000)
alpha <- matrix(c(NA, 0.05, NA,
                  NA, NA, NA,
                  0.01, 0.1, NA), nrow = 3, byrow = TRUE)
w <- matrix(c(0, 0.3, 0.3,
              0.4, 0, 0.4,
              0.3, 0.3, 0), nrow = 3, byrow = TRUE)
error3 <- matrix(nrow = 4, ncol = 4, dimnames = list(c("CX", "ER", "vanilla", "MNPO"), c("1->2", "3->1", "3->2", "obj.value")))
error3 <- rep(list(error3), 10)
time.list3 <- matrix(nrow = length(n.list), ncol = 4, dimnames = list(n.list, c("CX", "ER", "vanilla", "MNPO")))
for (i in 1:length(n.list)) {
  print(i)
  train.set <- generate_data(n=n.list[i], model.no = 3)
  x <- train.set$x
  y <- train.set$y

  test.set <- generate_data(n=2e4, model.no = 3)
  x.test <- test.set$x
  y.test <- test.set$y

  t1.CX <- proc.time()
  fit.npmc.CX <- try(npcs_confusion(x, y, algorithm = "CX", classifier = "logistic", w = w, alpha = alpha))
  D.CX <- duality_check_confusion(fit.npmc.CX, x, y, delta = 0.1)
  if (D.CX["f"] == 1) {
    y.pred.CX <- predict(fit.npmc.CX, x.test)
  }
  t2.CX <- proc.time()
  time.list3[i, "CX"] <- (t2.CX - t1.CX)["elapsed"]
  if (D.CX["f"] == 1) {
    er_matrix <- confusion_matrix(y.pred.CX, y.test)
    error3[[i]]["CX", 1] <- er_matrix[1,2]
    error3[[i]]["CX", 2] <- er_matrix[3,1]
    error3[[i]]["CX", 3] <- er_matrix[3,2]
    error3[[i]]["CX", 4] <- sum(w*er_matrix)
  } else {
    error3[[i]]["CX", ] <- rep(NA, 4)
  }


  t1.ER <- proc.time()
  fit.npmc.ER <- try(npcs_confusion(x, y, algorithm = "ER", classifier = "logistic", w = w, alpha = alpha, refit = FALSE))
  D.ER <- duality_check_confusion(fit.npmc.ER, x, y, delta = 0.2)
  if (D.ER["f"] == 1) {
    y.pred.ER <- predict(fit.npmc.ER, x.test)
  }
  t2.ER <- proc.time()
  time.list3[i, "ER"] <- (t2.ER - t1.ER)["elapsed"]
  if (D.ER["f"] == 1) {
    er_matrix <- confusion_matrix(y.pred.ER, y.test)
    error3[[i]]["ER", 1] <- er_matrix[1,2]
    error3[[i]]["ER", 2] <- er_matrix[3,1]
    error3[[i]]["ER", 3] <- er_matrix[3,2]
    error3[[i]]["ER", 4] <- sum(w*er_matrix)
  } else {
    error3[[i]]["ER", ] <- rep(NA, 4)
  }


  t1.vanilla <- proc.time()
  fit.vanilla <- data.frame(x = x, y = factor(y)) %>% multinom(y~., data = ., trace = FALSE)
  y.pred.vanilla <- data.frame(x = x.test) %>% predict(fit.vanilla,  newdata = .)
  t2.vanilla <- proc.time()
  time.list3[i, "vanilla"] <- (t2.vanilla - t1.vanilla)["elapsed"]
  er_matrix <- confusion_matrix(y.pred.vanilla, y.test)
  error3[[i]]["vanilla", 1] <- er_matrix[1,2]
  error3[[i]]["vanilla", 2] <- er_matrix[3,1]
  error3[[i]]["vanilla", 3] <- er_matrix[3,2]
  error3[[i]]["vanilla", 4] <- sum(w*er_matrix)


  t1.mnpo <- proc.time()
  fit.mnpo <- mnpo_confusion(x, y, classifier = "logistic", alpha = alpha, w = w, increment = 0.01)
  if (is.list(fit.mnpo)) {
    y.pred.mnpo <- predict_mnpo(fit.mnpo, x.test)
  }
  t2.mnpo <- proc.time()
  time.list3[i, "MNPO"] <- (t2.mnpo - t1.mnpo)["elapsed"]
  if (is.list(fit.mnpo)) {
    er_matrix <- confusion_matrix(y.pred.mnpo, y.test)
    error3[[i]]["MNPO", 1] <- er_matrix[1,2]
    error3[[i]]["MNPO", 2] <- er_matrix[3,1]
    error3[[i]]["MNPO", 3] <- er_matrix[3,2]
    error3[[i]]["MNPO", 4] <- sum(w*er_matrix)
  } else {
    error3[[i]]["MNPO", ] <- rep(NA, 4)
  }


}

# ---------------------------------------------------------
# model 4: imbalanced setting of model 1, "case 3" in the paper
# ---------------------------------------------------------
n.list <- seq(1000, 10000, 1000)
alpha <- c(0.15, 0.3, NA)
w <- c(0, 0, 1)
error4 <- matrix(nrow = 4, ncol = 3, dimnames = list(c("CX", "ER", "vanilla", "MNPO"), c("class 1", "class 2", "class 3")))
error4 <- rep(list(error4), 10)

for (i in 1:length(n.list)) {
  print(i)
  train.set <- generate_data(n=n.list[i], model.no = 4, prob = c(0.1, 0.1, 0.8))
  x <- train.set$x
  y <- train.set$y

  test.set <- generate_data(n=2e4, model.no = 4, prob = c(0.1, 0.1, 0.8))
  x.test <- test.set$x
  y.test <- test.set$y

  fit.npmc.CX <- try(npcs(x, y, algorithm = "CX", classifier = "logistic", w = w, alpha = alpha))
  D.CX <- duality_check(fit.npmc.CX, x, y, delta = 0.1)
  if (D.CX["f"] == 1) {
    y.pred.CX <- predict(fit.npmc.CX, x.test)
    error4[[i]]["CX", ] <- error_rate(y.pred.CX, y.test)
  } else {
    error4[[i]]["CX", ] <- rep(NA, 3)
  }


  fit.npmc.ER <- try(npcs(x, y, algorithm = "ER", classifier = "logistic", w = w, alpha = alpha, refit = FALSE))
  D.ER <- duality_check(fit.npmc.ER, x, y, delta = 0.2)
  if (D.ER["f"] == 1) {
    y.pred.ER <- predict(fit.npmc.ER, x.test)
    error4[[i]]["ER", ] <- error_rate(y.pred.ER, y.test)
  } else {
    error4[[i]]["ER", ] <- rep(NA, 3)
  }


  fit.vanilla <- data.frame(x = x, y = factor(y)) %>% multinom(y~., data = ., trace = FALSE)
  y.pred.vanilla <- data.frame(x = x.test) %>% predict(fit.vanilla,  newdata = .)
  error4[[i]]["vanilla", ] <- error_rate(y.pred.vanilla, y.test)

  fit.mnpo <- mnpo(x, y, classifier = "logistic", alpha = alpha, w = w, increment = 0.01)
  if (is.list(fit.mnpo)) {
    y.pred.mnpo <- predict_mnpo(fit.mnpo, x.test)
    error4[[i]]["MNPO", ] <- error_rate(y.pred.mnpo, y.test)
  } else {
    error4[[i]]["MNPO", ] <- rep(NA, 3)
  }
}



save(error1, error2, error3, error4, time.list1, time.list3, file = filename)







