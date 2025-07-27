ori_train <- read.csv("/Users/yetian/Library/CloudStorage/Dropbox/Columbia/Research/Project/MTL:TL-GMM/datasets/pen/pendigits-orig.tra")
registerDoParallel(12)
ori_train_sep <- strsplit(ori_train[,1]," ")
info_train <- foreach(i = 1:length(ori_train_sep), .combine = "rbind") %dopar% {
  if (ori_train_sep[[i]][1] == ".SEGMENT") {
    c(NA, as.numeric(tail(ori_train_sep[[i]], 1)))
  } else if (ori_train_sep[[i]][1] == ".COMMENT") {
    c(as.numeric(ori_train_sep[[i]][ori_train_sep[[i]] != ""][3]), NA)
  } else {
    rep(NA, 2)
  }
}
info_train_1 <- cbind(info_train[!is.na(info_train[, 1]), 1], info_train[!is.na(info_train[, 2]), 2])
colnames(info_train_1) <- c("user", "class")
info_train <- info_train_1
rownames(info_train) <- NULL
save(info_train, file = "/Users/yetian/Library/CloudStorage/Dropbox/Columbia/Research/Project/MTL:TL-GMM/datasets/pen/info_train.RData")


ori_test <- read.csv("/Users/yetian/Library/CloudStorage/Dropbox/Columbia/Research/Project/MTL:TL-GMM/datasets/pen/pendigits-orig.tes")
registerDoParallel(12)
ori_test_sep <- strsplit(ori_test[,1]," ")
info_test <- foreach(i = 1:length(ori_test_sep), .combine = "rbind") %dopar% {
  if (ori_test_sep[[i]][1] == ".SEGMENT") {
    c(NA, as.numeric(tail(ori_test_sep[[i]], 1)))
  } else if (ori_test_sep[[i]][1] == ".COMMENT") {
    c(as.numeric(ori_test_sep[[i]][ori_test_sep[[i]] != ""][3]), NA)
  } else {
    rep(NA, 2)
  }
}
info_test_1 <- cbind(info_test[!is.na(info_test[, 1]), 1], info_test[!is.na(info_test[, 2]), 2])
colnames(info_test_1) <- c("user", "class")
info_test <- info_test_1
rownames(info_test) <- NULL
save(info_test, file = "/Users/yetian/Library/CloudStorage/Dropbox/Columbia/Research/Project/MTL:TL-GMM/datasets/pen/info_test.RData")


