# NYU Greene

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

filename = paste(relative_path, "/output/simulation/knn_model1_check/", seed, ".RData", sep = "")
if (file.exists(filename)) {
  stop("Done!")
}

set.seed(0, kind = "L'Ecuyer-CMRG")


# -----------------------------------------------------------------------
train.set <- generate_data(n=1e5, model.no = 3)
x <- train.set$x
y <- train.set$y


# split data
split.ratio <- 0.5
ind <- Reduce("c", sapply(1:3, function(k){
  ind.k <- which(y == k)
  sample(ind.k, floor(length(ind.k)*split.ratio))
}, simplify = F))



# fit knn using training data
fit_knn <- knn3(x = x[-ind, ], y = factor(y)[-ind], k = floor(sqrt(length(y[-ind]))/3))



# NPMC
w <- c(0, 0, 1)
alpha1 <- seed/100
alpha <- c(alpha1, 0, NA)
D_matrix <- matrix(nrow = 100, ncol = 3, dimnames = list((1:100)/100, c("s", "f", "obj.value")))
for (i in 1:100) {
  print(i)
  alpha[2] <- i/100
  fit.npmc.ER <- try(npcs(x, y, algorithm = "ER", classifier = "knn", w = w, alpha = alpha, limit = 1000,
                          fitted.model = list(fit_knn, NA, ind), refit = FALSE))
  D <- duality_check(fit.npmc.ER, x, y, delta = 0.2, R.G = 10)
  D_matrix[i, 1:2] <- D
  if (D["f"] == 1) {
    fit.npmc.ER$obj.value <- min(fit.npmc.ER$obj.value, 1+0.2)
  }
  D_matrix[i, 3] <- fit.npmc.ER$obj.value
}




save(D_matrix, file = filename)

