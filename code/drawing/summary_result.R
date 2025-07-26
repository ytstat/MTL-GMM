library(dplyr)
library(ggplot2)
library(ggpubr)
library(mvtnorm)
library(latex2exp)
library(here)
library(conflicted)
relative_path <- here()

conflict_prefer(name = "select", winner = "dplyr")
conflict_prefer(name = "filter", winner = "dplyr")
conflict_prefer(name = "margin", winner = "ggplot2")

# ------------------------------------
# strong duality and feasibility test in model 1
# -------------------------------------

## underlying truth: Figure 2, upper left plot
# -------------------------------------
f <- function(alpha1) {
  pnorm(-sqrt(sum((c(0,1,0,1,0) -c(-1,2,-1,1,1))^2))-qnorm(alpha1))
}


alpha1 <- seq(0.001, 1, 0.001)
alpha2 <- f(alpha1)


plot_verify <- ggplot(data = data.frame(alpha1, alpha2), aes(x = alpha1, y = alpha2)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")+ ggtitle("True") +
  geom_ribbon(aes(ymin=0, ymax=0, fill = "s = 0, f = 1")) + geom_ribbon(aes(ymin=0, ymax=alpha2, fill = "s = 1, f = 0")) +
  geom_ribbon(aes(ymin=alpha2, ymax=1, fill="s = 1, f = 1")) +geom_ribbon(aes(ymin=0, ymax=0, fill = "s = 0, f = 0")) +
  xlab(TeX(r"($\alpha_1$)")) + ylab(TeX(r"($\alpha_2$)")) +
  guides(fill = guide_legend(nrow = 2)) +
  coord_cartesian(ylim = c(0, 1)) + geom_line(size = 1, color = '#abd9e9') +
  scale_fill_manual("Strong duality and feasibility",values=c("s = 0, f = 1" = '#abd9e9',
                                                              "s = 0, f = 0" = '#d7191c',
                                                              "s = 1, f = 0" = '#fdae61',
                                                              "s = 1, f = 1" = '#2c7bb6'),
                    breaks = c("s = 1, f = 0",
                               "s = 0, f = 0",
                               "s = 0, f = 1",
                               "s = 1, f = 1"))


## NPMC-CX-logistic: Figure 2, upper right plot
# -------------------------------------
s.matrix <- matrix(nrow = 100, ncol = 100, dimnames = list((1:100)/100, (1:100)/100))
f.matrix <- matrix(nrow = 100, ncol = 100, dimnames = list((1:100)/100, (1:100)/100))
f.verify.matrix <- matrix(nrow = 100, ncol = 100, dimnames = list((1:100)/100, (1:100)/100))
obj.verify.matrix <- matrix(nrow = 100, ncol = 100, dimnames = list((1:100)/100, (1:100)/100))
obj.matrix <- matrix(nrow = 100, ncol = 100, dimnames = list((1:100)/100, (1:100)/100))
for (i in 1:100) {
  load(paste0(relative_path, relative_path, "/out/simulation/logistic_model1_check/", i, ".RData"))
  s.matrix[i, ] <- D_matrix[, "s"]
  f.matrix[i, ] <- D_matrix[, "f"]
  obj.matrix[i, ] <- D_matrix[, "obj.value"]
}

data <- expand.grid(alpha1=(1:100)/100, alpha2=(1:100)/100)
data$s <- as.vector(s.matrix)
data$f <- as.vector(f.matrix)
data$obj_value <- as.vector(obj.matrix)
data[data$f == 0, "obj_value"] <- NA


data[, "combination"] <- NA
data[data$s == 0 & data$f == 0, "combination"] <- "s = 0, f = 0"
data[data$s == 1 & data$f == 0, "combination"] <- "s = 1, f = 0"
data[data$s == 0 & data$f == 1, "combination"] <- "s = 0, f = 1"
data[data$s == 1 & data$f == 1, "combination"] <- "s = 1, f = 1"
data[, "combination"] <- factor(data[, "combination"], levels = c("s = 1, f = 0", "s = 0, f = 0", "s = 0, f = 1", "s = 1, f = 1"))



# Heatmap
plot_left <- ggplot(data, aes(alpha1, alpha2, fill = (obj_value)**(1/4))) + geom_tile() +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
  xlab(TeX(r"($\alpha_1$)")) + ylab(TeX(r"($\alpha_2$)")) + labs(fill='objective value') +
  geom_point(aes(size=""), shape =NA, colour = "darkgray")+
  guides(size=guide_legend(title=TeX(r"($\hat{f} = 0$)"), title.vjust = 1, override.aes=list(shape=15, size = 7)),
         fill=guide_colorbar(title.vjust = 0.8)) + scale_fill_continuous(limits = range(0, 1.2))

plot_right_logistic <- data %>% ggplot(aes(alpha1, alpha2, fill= combination)) + geom_tile() +
  theme(plot.title = element_text(hjust = 0.5), legend.position="none") + ggtitle("Predicted by NPMC-CX-Logistic") +
  guides(fill = guide_legend(nrow = 2)) + xlab(TeX(r"($\alpha_1$)")) + ylab(TeX(r"($\alpha_2$)")) +
  scale_fill_manual(values= c("s = 0, f = 0" = '#d7191c',
                              "s = 1, f = 0" = '#fdae61',
                              "s = 0, f = 1" = '#abd9e9',
                              "s = 1, f = 1" = '#2c7bb6'),
                    labels=c(TeX(r"($\hat{s}=1, \hat{f}=0$)"), TeX(r"($\hat{s}=0, \hat{f}=0$)"),
                             TeX(r"($\hat{s}=0, \hat{f}=1$)"), TeX(r"($\hat{s}=1, \hat{f}=1$)")),
                    drop=FALSE) +
  labs(fill='strong duality and feasibility')

# # save as a 10*5 pdf
# ggarrange(plot_left, plot_right, nrow = 1, ncol = 2)

## NPMC-ER-knn: Figure 2, bottom left plot
# -------------------------------------
s.matrix <- matrix(nrow = 100, ncol = 100, dimnames = list((1:100)/100, (1:100)/100))
f.matrix <- matrix(nrow = 100, ncol = 100, dimnames = list((1:100)/100, (1:100)/100))
obj.matrix <- matrix(nrow = 100, ncol = 100, dimnames = list((1:100)/100, (1:100)/100))
for (i in 1:100) {
  load(paste0(relative_path, relative_path, "/output/simulation/knn_model1_check/", i, ".RData"))
  s.matrix[i, ] <- D_matrix[, "s"]
  f.matrix[i, ] <- D_matrix[, "f"]
  obj.matrix[i, ] <- D_matrix[, "obj.value"]
}

data <- expand.grid(alpha1=(1:100)/100, alpha2=(1:100)/100)
data$s <- as.vector(s.matrix)
data$f <- as.vector(f.matrix)
data$obj_value <- as.vector(obj.matrix)
data[data$f == 0, "obj_value"] <- NA

data[, "combination"] <- NA
data[data$s == 0 & data$f == 0, "combination"] <- "s = 0, f = 0"
data[data$s == 1 & data$f == 0, "combination"] <- "s = 1, f = 0"
data[data$s == 0 & data$f == 1, "combination"] <- "s = 0, f = 1"
data[data$s == 1 & data$f == 1, "combination"] <- "s = 1, f = 1"
data[, "combination"] <- factor(data[, "combination"], levels = c("s = 1, f = 0", "s = 0, f = 0", "s = 0, f = 1", "s = 1, f = 1"))



# Heatmap
plot_left <- ggplot(data, aes(alpha1, alpha2, fill = obj_value**(1/4))) + geom_tile() +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
  xlab(TeX(r"($\alpha_1$)")) + ylab(TeX(r"($\alpha_2$)")) + labs(fill='objective value') +
  geom_point(aes(size=""), shape =NA, colour = "darkgray")+
  guides(size=guide_legend(title=TeX(r"($\hat{f} = 0$)"), title.vjust = 1, override.aes=list(shape=15, size = 7)),
         fill=guide_colorbar(title.vjust = 0.8)) + scale_fill_continuous(limits = range(0, 1.2))

plot_right_knn <- data %>% ggplot(aes(alpha1, alpha2, fill= combination)) + geom_tile() +
  theme(plot.title = element_text(hjust = 0.5), legend.position="none") + ggtitle("Predicted by NPMC-ER-kNN") +
  guides(fill = guide_legend(nrow = 2)) + xlab(TeX(r"($\alpha_1$)")) + ylab(TeX(r"($\alpha_2$)")) +
  scale_fill_manual(values= c("s = 0, f = 0" = '#d7191c',
                              "s = 1, f = 0" = '#fdae61',
                              "s = 0, f = 1" = '#abd9e9',
                              "s = 1, f = 1" = '#2c7bb6'),
                    labels=c(TeX(r"($\hat{s}=1, \hat{f}=0$)"), TeX(r"($\hat{s}=0, \hat{f}=0$)"),
                             TeX(r"($\hat{s}=0, \hat{f}=1$)"), TeX(r"($\hat{s}=1, \hat{f}=1$)")),
                    drop=FALSE) +
  labs(fill='strong duality and feasibility')


## draw the combined plot: Figure 2
# -------------------------------------
plot_legend <- data %>% ggplot(aes(alpha1, alpha2, fill= combination)) +  lims(x = c(0,0), y = c(0,0))+
  theme_void() +
  theme(legend.position = c(0.35,0.5), legend.spacing.y = unit(0.2, 'cm'), legend.title = element_text(size = 12)) +
  geom_ribbon(aes(ymin=0, ymax=0, fill = "s = 0, f = 1")) + geom_ribbon(aes(ymin=0, ymax=0, fill = "s = 1, f = 0")) +
  geom_ribbon(aes(ymin=0, ymax=0, fill="s = 1, f = 1")) +geom_ribbon(aes(ymin=0, ymax=0, fill = "s = 0, f = 0")) +
  guides(fill = guide_legend(byrow = TRUE)) +
  scale_fill_manual(values= c("s = 0, f = 0" = '#d7191c',
                              "s = 1, f = 0" = '#fdae61',
                              "s = 0, f = 1" = '#abd9e9',
                              "s = 1, f = 1" = '#2c7bb6'),
                    labels=c("", "", "", ""),
                    breaks = c("s = 1, f = 0",
                               "s = 0, f = 0",
                               "s = 0, f = 1",
                               "s = 1, f = 1"),
                    drop=FALSE) +
  labs(fill='strong duality and feasibility')

# save as a 10*5 pdf
ggarrange(plot_verify, plot_right_logistic, plot_right_knn, plot_legend, nrow = 2, ncol = 2)

# ------------------------------------
# model 1: "case 1" in the paper
# ------------------------------------
## logistic
er.table1 <- data.frame(method = NA, class = NA, n = NA, value = NA)
n.list <- seq(1000, 10000, 1000)
n.plot <- seq(1000, 10000, 2000)
time.list.avg.logistic <- matrix(0, nrow = 10, ncol = 4, dimnames = list(seq(1000, 10000, 1000), c("CX", "ER", "vanilla", "MNPO")))
for (i in 1:500) {
  load(paste0(relative_path, relative_path, "/output/simulation/logistic/", i, ".RData"))
  for (j in 1:10) {
    er.table1 <- rbind(er.table1, data.frame(method = rep(c("CX", "ER", "vanilla", "MNPO"), 3), class = rep(colnames(error1[[j]]), each = 4),n = n.list[[j]], value = as.vector(error1[[j]])))
  }
  time.list.avg.logistic <- time.list.avg.logistic + time.list1
}
time.list.avg.logistic <- time.list.avg.logistic/500
er.table1 <- er.table1[-1,]
er.table1$n <- as.factor(er.table1$n)
plot1.logistic <- er.table1 %>% filter(method == "CX", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("NPMC-CX-logistic") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.15, col = "#E69F00") + geom_hline(yintercept = 0.3, col = "#56B4E9") + labs(color='error')
plot2.logistic <- er.table1 %>% filter(method == "ER", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("NPMC-ER-logistic") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.15, col = "#E69F00") + geom_hline(yintercept = 0.3, col = "#56B4E9")  + coord_cartesian(ylim = c(0, 0.85), clip = "off")+
  annotate("text", x = 1, y = -0.145, size = 2.8, label = c("(2)")) + labs(color='error')
plot3.logistic <- er.table1 %>% filter(method == "vanilla", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("vanilla-logistic") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.15, col = "#E69F00") + geom_hline(yintercept = 0.3, col = "#56B4E9") + labs(color='error')
plot4.logistic <- er.table1 %>% filter(method == "MNPO", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("MNPO-logistic") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) + coord_cartesian(ylim = c(0, 0.85), clip = "off")+
  geom_hline(yintercept = 0.15, col = "#E69F00") + geom_hline(yintercept = 0.3, col = "#56B4E9") +
  annotate("text", x = 1, y = -0.145, size = 2.8, label = c("(3)")) + labs(color='error')

er.table1 %>% filter(method == "CX", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/3)
er.table1 %>% filter(method == "ER", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/3)
er.table1 %>% filter(method == "vanilla", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/3)
er.table1 %>% filter(method == "MNPO", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/3)


# ggarrange(plot1.logistic, plot2.logistic, plot3.logistic, plot4.logistic, nrow = 1, common.legend = TRUE, legend = "bottom")

## LDA
er.table1 <- data.frame(method = NA, class = NA, n = NA, value = NA)
n.list <- seq(1000, 10000, 1000)
n.plot <- seq(1000, 10000, 2000)
time.list.avg.lda <- matrix(0, nrow = 10, ncol = 4, dimnames = list(seq(1000, 10000, 1000), c("CX", "ER", "vanilla", "MNPO")))
for (i in 1:500) {
  load(paste0(relative_path, relative_path, "/output/simulation/lda/", i, ".RData"))
  for (j in 1:10) {
    er.table1 <- rbind(er.table1, data.frame(method = rep(c("CX", "ER", "vanilla", "MNPO"), 3), class = rep(colnames(error1[[j]]), each = 4),n = n.list[[j]], value = as.vector(error1[[j]])))
  }
  time.list.avg.lda <- time.list.avg.lda + time.list1
}
time.list.avg.lda <- time.list.avg.lda/500
er.table1 <- er.table1[-1,]
er.table1$n <- as.factor(er.table1$n)
plot1.lda <- er.table1 %>% filter(method == "CX", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("NPMC-CX-LDA") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.15, col = "#E69F00") + geom_hline(yintercept = 0.3, col = "#56B4E9") + labs(color='error')
plot2.lda <- er.table1 %>% filter(method == "ER", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("NPMC-ER-LDA") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.15, col = "#E69F00") + geom_hline(yintercept = 0.3, col = "#56B4E9") +
  annotate("text", x = 1, y = -0.168, size = 2.8, label = c("(1)")) + coord_cartesian(ylim = c(0, 1), clip = "off") + labs(color='error')
plot3.lda <- er.table1 %>% filter(method == "vanilla", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("vanilla-LDA") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.15, col = "#E69F00") + geom_hline(yintercept = 0.3, col = "#56B4E9") + labs(color='error')
plot4.lda <- er.table1 %>% filter(method == "MNPO", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("MNPO-LDA") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.15, col = "#E69F00") + geom_hline(yintercept = 0.3, col = "#56B4E9") + labs(color='error')

er.table1 %>% filter(method == "CX", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/3)
er.table1 %>% filter(method == "ER", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/3)
er.table1 %>% filter(method == "vanilla", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/3)
er.table1 %>% filter(method == "MNPO", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/3)

# ggarrange(plot1.lda, plot2.lda, plot3.lda, plot4.lda, nrow = 1, common.legend = TRUE, legend = "bottom")

## knn
er.table1 <- data.frame(method = NA, class = NA, n = NA, value = NA)
n.list <- seq(1000, 10000, 1000)
n.plot <- seq(1000, 10000, 2000)
time.list.avg.knn <- matrix(0, nrow = 10, ncol = 4, dimnames = list(seq(1000, 10000, 1000), c("CX", "ER", "vanilla", "MNPO")))
for (i in 1:500) {
  load(paste0(relative_path, relative_path, "/output/simulation/knn/", i, ".RData"))
  for (j in 1:10) {
    er.table1 <- rbind(er.table1, data.frame(method = rep(c("CX", "ER", "vanilla", "MNPO"), 3), class = rep(colnames(error1[[j]]), each = 4),n = n.list[[j]], value = as.vector(error1[[j]])))
  }
  time.list.avg.knn <- time.list.avg.knn + time.list1
}
time.list.avg.knn <- time.list.avg.knn/500
er.table1 <- er.table1[-1,]
er.table1$n <- as.factor(er.table1$n)
plot1.knn <- er.table1 %>% filter(method == "CX", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("NPMC-CX-kNN") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.15, col = "#E69F00") + geom_hline(yintercept = 0.3, col = "#56B4E9") + labs(color='error')
plot2.knn <- er.table1 %>% filter(method == "ER", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("NPMC-ER-kNN") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.15, col = "#E69F00") + geom_hline(yintercept = 0.3, col = "#56B4E9") +
  annotate("text", x = 1, y = -0.17, size = 2.8, label = c("(54)")) + coord_cartesian(ylim = c(0, 1), clip = "off") + labs(color='error')
plot3.knn <- er.table1 %>% filter(method == "vanilla", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("vanilla-kNN") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.15, col = "#E69F00") + geom_hline(yintercept = 0.3, col = "#56B4E9") + labs(color='error')
plot4.knn <- er.table1 %>% filter(method == "MNPO", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("MNPO-kNN") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.15, col = "#E69F00") + geom_hline(yintercept = 0.3, col = "#56B4E9") +
  annotate("text", x = 1:2, y = -0.17, size = 2.8, label = c("(83)", "(1)")) + coord_cartesian(ylim = c(0, 1), clip = "off") + labs(color='error')

er.table1 %>% filter(method == "CX", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/3)
er.table1 %>% filter(method == "ER", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/3)
er.table1 %>% filter(method == "vanilla", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/3)
er.table1 %>% filter(method == "MNPO", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/3)

# ggarrange(plot1.knn, plot2.knn, plot3.knn, plot4.knn, nrow = 1, common.legend = TRUE, legend = "bottom")


## nnb
er.table1 <- data.frame(method = NA, class = NA, n = NA, value = NA)
n.list <- seq(1000, 10000, 1000)
n.plot <- seq(1000, 10000, 2000)
time.list.avg.nnb <- matrix(0, nrow = 10, ncol = 4, dimnames = list(seq(1000, 10000, 1000), c("CX", "ER", "vanilla", "MNPO")))
for (i in 1:500) {
  load(paste0(relative_path, relative_path, "/output/simulation/nnb/", i, ".RData"))
  for (j in 1:10) {
    er.table1 <- rbind(er.table1, data.frame(method = rep(c("CX", "ER", "vanilla", "MNPO"), 3), class = rep(colnames(error1[[j]]), each = 4),n = n.list[[j]], value = as.vector(error1[[j]])))
  }
  time.list.avg.nnb <- time.list.avg.nnb + time.list1
}
time.list.avg.nnb <- time.list.avg.nnb/500
er.table1 <- er.table1[-1,]
er.table1$n <- as.factor(er.table1$n)
plot1.nnb <- er.table1 %>% filter(method == "CX", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color = class)) +
  ggtitle("NPMC-CX-NNB") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.15, col = "#E69F00") + geom_hline(yintercept = 0.3, col = "#56B4E9") + labs(color='error')
plot2.nnb <- er.table1 %>% filter(method == "ER", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("NPMC-ER-NNB") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.15, col = "#E69F00") + geom_hline(yintercept = 0.3, col = "#56B4E9") +
  annotate("text", x = 1, y = -0.17, size = 2.8, label = c("(4)")) + coord_cartesian(ylim = c(0, 1), clip = "off") + labs(color='error')
plot3.nnb <- er.table1 %>% filter(method == "vanilla", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("vanilla-NNB") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8))  +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.15, col = "#E69F00") + geom_hline(yintercept = 0.3, col = "#56B4E9") + labs(color='error')
plot4.nnb <- er.table1 %>% filter(method == "MNPO", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("MNPO-NNB") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8))  +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.15, col = "#E69F00") + geom_hline(yintercept = 0.3, col = "#56B4E9") +
  annotate("text", x = 1, y = -0.17, size = 2.8, label = c("(8)")) + coord_cartesian(ylim = c(0, 1), clip = "off") + labs(color='error')


er.table1 %>% filter(method == "CX", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/3)
er.table1 %>% filter(method == "ER", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/3)
er.table1 %>% filter(method == "vanilla", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/3)
er.table1 %>% filter(method == "MNPO", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/3)

# ggarrange(plot1.nnb, plot2.nnb, plot3.nnb, plot4.nnb, nrow = 1, common.legend = TRUE, legend = "bottom")

## output: 13 times 11 PDF: model1.pdf  --- Figure 1
ggarrange(plot1.logistic, plot2.logistic, plot3.logistic,
          plot1.lda, plot2.lda, plot3.lda,
          plot1.knn, plot2.knn, plot3.knn,
          plot1.nnb, plot2.nnb, plot3.nnb, nrow = 4, ncol = 3, common.legend = TRUE, legend = "bottom")

## output: 13 times 3 PDF: model1_mnpo.pdf  --- Figure 7
ggarrange(plot4.logistic, plot4.lda, plot4.knn, plot4.nnb, nrow = 1, common.legend = TRUE, legend = "bottom")


## plot the computational time
time.avg.log.logistic <- data.frame(expand.grid(n = factor(seq(1000, 10000, 1000)), method = factor(c("NPMC-CX", "NPMC-ER", "vanilla", "MNPO"), levels = c("NPMC-CX", "NPMC-ER", "vanilla", "MNPO"))), time = as.vector(log(time.list.avg.logistic)))
time.avg.log.lda<- data.frame(expand.grid(n = factor(seq(1000, 10000, 1000)), method = factor(c("NPMC-CX", "NPMC-ER", "vanilla", "MNPO"), levels = c("NPMC-CX", "NPMC-ER", "vanilla", "MNPO"))), time = as.vector(log(time.list.avg.lda)))
time.avg.log.knn <- data.frame(expand.grid(n = factor(seq(1000, 10000, 1000)), method = factor(c("NPMC-CX", "NPMC-ER", "vanilla", "MNPO"), levels = c("NPMC-CX", "NPMC-ER", "vanilla", "MNPO"))), time = as.vector(log(time.list.avg.knn)))
time.avg.log.nnb <- data.frame(expand.grid(n = factor(seq(1000, 10000, 1000)), method = factor(c("NPMC-CX", "NPMC-ER", "vanilla", "MNPO"), levels = c("NPMC-CX", "NPMC-ER", "vanilla", "MNPO"))), time = as.vector(log(time.list.avg.nnb)))


plot.time.logistic <- time.avg.log.logistic %>% ggplot(aes(x=n, y=time, color = method, group = method)) + geom_point(size = 2) +
  geom_line() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Logistic") + scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "mediumpurple2")) +
  ylab("logarithm time (in seconds)")

plot.time.lda <- time.avg.log.lda %>% ggplot(aes(x=n, y=time, color = method, group = method)) + geom_point(size = 2) +
  geom_line() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("LDA") + scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "mediumpurple2")) +
  ylab("logarithm time (in seconds)")

plot.time.knn <- time.avg.log.knn %>% ggplot(aes(x=n, y=time, color = method, group = method)) + geom_point(size = 2) +
  geom_line() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("kNN") + scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "mediumpurple2")) +
  ylab("logarithm time (in seconds)")

plot.time.nnb<- time.avg.log.nnb %>% ggplot(aes(x=n, y=time, color = method, group = method)) + geom_point(size = 2) +
  geom_line() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Logistic") + scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "mediumpurple2")) +
  ylab("logarithm time (in seconds)")

## output: 10 times 7 PDF: model1_time.pdf --- Figure 8
ggarrange(plot.time.logistic, plot.time.lda, plot.time.knn, plot.time.nnb, nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")


# ------------------------------------
# model 2: "case 2" in the paper
# ------------------------------------
## logistic
er.table2 <- data.frame(method = NA, class = NA, n = NA, value = NA)
n.list <- seq(1000, 10000, 1000)
n.plot <- seq(1000, 10000, 2000)
for (i in 1:500) {
  load(paste0(relative_path, relative_path, "/output/simulation/logistic/", i, ".RData"))
  for (j in 1:10) {
    er.table2 <- rbind(er.table2, data.frame(method = rep(c("CX", "ER", "vanilla"), 5), class = rep(colnames(error2[[j]]), each = 3),n = n.list[[j]], value = as.vector(error2[[j]])))
  }
}
er.table2 <- er.table2[-1,]
er.table2$n <- as.factor(er.table2$n)
plot1.logistic <- er.table2 %>% filter(method == "CX", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("NPMC-CX-logistic") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "mediumpurple2", "gray43"),
                     labels = c(paste("class", 1:4), "objective value")) +
  geom_hline(yintercept = 0.04, col = "#E69F00")  + geom_hline(yintercept = 0.08, col = "#009E73") + labs(color='error')
plot2.logistic <- er.table2 %>% filter(method == "ER", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("NPMC-ER-logistic") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "mediumpurple2", "gray43"),
                     labels = c(paste("class", 1:4), "objective value")) + coord_cartesian(ylim = c(0, 0.65), clip = "off") +
  geom_hline(yintercept = 0.04, col = "#E69F00") + geom_hline(yintercept = 0.08, col = "#009E73") +
  annotate("text", x = 1:2, y = -0.110, size = 2.8, label = c("(98)", "(2)")) + labs(color='error')
plot3.logistic <- er.table2 %>% filter(method == "vanilla", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("vanilla-logistic") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "mediumpurple2", "gray43"),
                     labels = c(paste("class", 1:4), "objective value")) +
  geom_hline(yintercept = 0.04, col = "#E69F00") + geom_hline(yintercept = 0.08, col = "#009E73") + labs(color='error')


er.table2 %>% filter(method == "CX", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/5)
er.table2 %>% filter(method == "ER", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/5)
er.table2 %>% filter(method == "vanilla", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/5)

# ggarrange(plot1.logistic, plot2.logistic, plot3.logistic, nrow = 1, common.legend = TRUE, legend = "bottom")

## LDA
er.table2 <- data.frame(method = NA, class = NA, n = NA, value = NA)
n.list <- seq(1000, 10000, 1000)
n.plot <- seq(1000, 10000, 2000)
for (i in 1:500) {
  load(paste0(relative_path, "/output/simulation/lda/", i, ".RData"))
  for (j in 1:10) {
    er.table2 <- rbind(er.table2, data.frame(method = rep(c("CX", "ER", "vanilla"), 5), class = rep(colnames(error2[[j]]), each = 3),n = n.list[[j]], value = as.vector(error2[[j]])))
  }
}
er.table2 <- er.table2[-1,]
er.table2$n <- as.factor(er.table2$n)
plot1.lda <- er.table2 %>% filter(method == "CX", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("NPMC-CX-LDA") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "mediumpurple2", "gray43"),
                     labels = c(paste("class", 1:4), "objective value")) +
  geom_hline(yintercept = 0.04, col = "#E69F00") + geom_hline(yintercept = 0.08, col = "#009E73") +
  annotate("text", x = 1, y = -0.15, size = 2.8, label = c("(24)")) + coord_cartesian(ylim = c(0, 0.9), clip = "off") + labs(color='error')
plot2.lda <- er.table2 %>% filter(method == "ER", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("NPMC-ER-LDA") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "mediumpurple2", "gray43"),
                     labels = c(paste("class", 1:4), "objective value")) +
  geom_hline(yintercept = 0.04, col = "#E69F00") + geom_hline(yintercept = 0.08, col = "#009E73") +
  annotate("text", x = 1, y = -0.104, size = 2.8, label = c("(14)")) + coord_cartesian(ylim = c(0, 0.6), clip = "off") + labs(color='error')
plot3.lda <- er.table2 %>% filter(method == "vanilla", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("vanilla-LDA") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "mediumpurple2", "gray43"),
                     labels = c(paste("class", 1:4), "objective value")) +
  geom_hline(yintercept = 0.04, col = "#E69F00") + geom_hline(yintercept = 0.08, col = "#009E73") + labs(color='error')

er.table2 %>% filter(method == "CX", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/5)
er.table2 %>% filter(method == "ER", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/5)
er.table2 %>% filter(method == "vanilla", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/5)


# ggarrange(plot1.lda, plot2.lda, plot3.lda, nrow = 1, common.legend = TRUE, legend = "bottom")

## knn
er.table2 <- data.frame(method = NA, class = NA, n = NA, value = NA)
n.list <- seq(1000, 10000, 1000)
n.plot <- seq(1000, 10000, 2000)
for (i in 1:500) {
  load(paste0(relative_path, "/output/simulation/knn/", i, ".RData"))
  for (j in 1:10) {
    er.table2 <- rbind(er.table2, data.frame(method = rep(c("CX", "ER", "vanilla"), 5), class = rep(colnames(error2[[j]]), each = 3),n = n.list[[j]], value = as.vector(error2[[j]])))
  }
}
er.table2 <- er.table2[-1,]
er.table2$n <- as.factor(er.table2$n)
plot1.knn <- er.table2 %>% filter(method == "CX", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("NPMC-CX-kNN") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) + coord_cartesian(ylim = c(0, 0.55), clip = "off") +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "mediumpurple2", '#8da0cb'),
                     labels = c(paste("class", 1:4), "objective value")) +
  geom_hline(yintercept = 0.04, col = "#E69F00") + geom_hline(yintercept = 0.08, col = "#009E73") +
  annotate("text", x = 1:5, y = -0.10, size = 2.8, label = c("(117)", "(39)", "(7)", "(2)", "(2)")) + labs(color='error')
plot2.knn <- er.table2 %>% filter(method == "ER", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("NPMC-ER-kNN") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "mediumpurple2", '#8da0cb'),
                     labels = c(paste("class", 1:4), "objective value")) +
  geom_hline(yintercept = 0.04, col = "#E69F00") + geom_hline(yintercept = 0.08, col = "#009E73") +
  annotate("text", x = 1:3, y = -0.09, size = 2.8, label = c("(118)", "(8)", "(1)")) + coord_cartesian(ylim = c(0, 0.52), clip = "off") + labs(color='error')
plot3.knn <- er.table2 %>% filter(method == "vanilla", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("vanilla-kNN") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8))  +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "mediumpurple2", '#8da0cb'),
                     labels = c(paste("class", 1:4), "objective value")) +
  geom_hline(yintercept = 0.04, col = "#E69F00") + geom_hline(yintercept = 0.08, col = "#009E73") + labs(color='error')


er.table2 %>% filter(method == "CX", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/5)
er.table2 %>% filter(method == "ER", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/5)
er.table2 %>% filter(method == "vanilla", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/5)

# ggarrange(plot1.knn, plot2.knn, plot3.knn, nrow = 1, common.legend = TRUE, legend = "bottom")

## NNB
er.table2 <- data.frame(method = NA, class = NA, n = NA, value = NA)
n.list <- seq(1000, 10000, 1000)
n.plot <- seq(1000, 10000, 2000)
for (i in 1:500) {
  load(paste0(relative_path, "/output/simulation/nnb/", i, ".RData"))
  for (j in 1:10) {
    er.table2 <- rbind(er.table2, data.frame(method = rep(c("CX", "ER", "vanilla"), 5), class = rep(colnames(error2[[j]]), each = 3),n = n.list[[j]], value = as.vector(error2[[j]])))
  }
}
er.table2 <- er.table2[-1,]
er.table2$n <- as.factor(er.table2$n)
plot1.nnb <- er.table2 %>% filter(method == "CX", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("NPMC-CX-NNB") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "mediumpurple2", "gray43"),
                     labels = c(paste("class", 1:4), "objective value")) +
  geom_hline(yintercept = 0.04, col = "#E69F00") + geom_hline(yintercept = 0.08, col = "#009E73") +
  annotate("text", x = 1:3, y = -0.172, size = 2.8, label = c("(25)", "(2)", "(1)")) + coord_cartesian(ylim = c(0, 1), clip = "off") + labs(color='error')
plot2.nnb <- er.table2 %>% filter(method == "ER", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("NPMC-ER-NNB") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "mediumpurple2", "gray43"),
                     labels = c(paste("class", 1:4), "objective value")) +
  geom_hline(yintercept = 0.04, col = "#E69F00") + geom_hline(yintercept = 0.08, col = "#009E73") +
  annotate("text", x = 1:2, y = -0.165, size = 2.8, label = c("(34)", "(3)")) + coord_cartesian(ylim = c(0, 0.9), clip = "off") + labs(color='error')
plot3.nnb <- er.table2 %>% filter(method == "vanilla", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("vanilla-NNB") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "mediumpurple2", "gray43"),
                     labels = c(paste("class", 1:4), "objective value")) +
  geom_hline(yintercept = 0.04, col = "#E69F00") + geom_hline(yintercept = 0.08, col = "#009E73") + labs(color='error')

er.table2 %>% filter(method == "CX", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/5)
er.table2 %>% filter(method == "ER", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/5)
er.table2 %>% filter(method == "vanilla", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/5)

# ggarrange(plot1.nnb, plot2.nnb, plot3.nnb, nrow = 1, common.legend = TRUE, legend = "bottom")


# output: 13 times 11 PDF --- Figure 9
ggarrange(plot1.logistic, plot2.logistic, plot3.logistic,
          plot1.lda, plot2.lda, plot3.lda,
          plot1.knn, plot2.knn, plot3.knn,
          plot1.nnb, plot2.nnb, plot3.nnb, nrow = 4, ncol = 3, common.legend = TRUE, legend = "bottom")


# ------------------------------------
# model 3: GNPMC version of model 1, "case 4" in the paper
# ------------------------------------
## logistic
er.table3 <- data.frame(method = NA, class = NA, n = NA, value = NA)
n.list <- seq(1000, 10000, 1000)
n.plot <- seq(1000, 10000, 2000)
time.list.avg.logistic <- matrix(0, nrow = 10, ncol = 4, dimnames = list(seq(1000, 10000, 1000), c("CX", "ER", "vanilla", "MNPO")))
for (i in 1:500) {
  load(paste0(relative_path, "/output/simulation/logistic/", i, ".RData"))
  for (j in 1:10) {
    er.table3 <- rbind(er.table3, data.frame(method = rep(c("CX", "ER", "vanilla", "MNPO"), 4), class = rep(colnames(error3[[j]]), each = 4),n = n.list[[j]], value = as.vector(error3[[j]])))
  }
  time.list.avg.logistic <- time.list.avg.logistic + time.list3
}
time.list.avg.logistic <- time.list.avg.logistic/500
er.table3 <- er.table3[-1,]
er.table3$n <- as.factor(er.table3$n)
plot1.logistic <- er.table3 %>% filter(method == "CX", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("GNPMC-CX-logistic") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "gray43"),
                     labels = c(TeX(r"($P(\phi(X) = 2|Y=1)$)"), TeX(r"($P(\phi(X) = 1|Y=3)$)"), TeX(r"($P(\phi(X) = 2|Y=3)$)"), "objective value")) +
  geom_hline(yintercept = 0.05, col = "#E69F00") + geom_hline(yintercept = 0.01, col = "#56B4E9") + geom_hline(yintercept = 0.1, col = "#009E73") +
  labs(color='error')
plot2.logistic <- er.table3 %>% filter(method == "ER", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("GNPMC-ER-logistic") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "gray43"),
                     labels = c(TeX(r"($P(\phi(X) = 2|Y=1)$)"), TeX(r"($P(\phi(X) = 1|Y=3)$)"), TeX(r"($P(\phi(X) = 2|Y=3)$)"), "objective value")) +
  geom_hline(yintercept = 0.05, col = "#E69F00") + geom_hline(yintercept = 0.01, col = "#56B4E9") + geom_hline(yintercept = 0.1, col = "#009E73") +
  labs(color='error')
plot3.logistic <- er.table3 %>% filter(method == "vanilla", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("vanilla-logistic") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "gray43"),
                     labels = c(TeX(r"($P(\phi(X) = 2|Y=1)$)"), TeX(r"($P(\phi(X) = 1|Y=3)$)"), TeX(r"($P(\phi(X) = 2|Y=3)$)"), "objective value")) +
  geom_hline(yintercept = 0.05, col = "#E69F00") + geom_hline(yintercept = 0.01, col = "#56B4E9") + geom_hline(yintercept = 0.1, col = "#009E73") +
  labs(color='error')
plot4.logistic <- er.table3 %>% filter(method == "MNPO", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("MNPO-logistic") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "gray43"),
                     labels = c(TeX(r"($P(\phi(X) = 2|Y=1)$)"), TeX(r"($P(\phi(X) = 1|Y=3)$)"), TeX(r"($P(\phi(X) = 2|Y=3)$)"), "objective value")) +
  geom_hline(yintercept = 0.05, col = "#E69F00") + geom_hline(yintercept = 0.01, col = "#56B4E9") + geom_hline(yintercept = 0.1, col = "#009E73") +
  labs(color='error')


er.table3 %>% filter(method == "CX", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/4)
er.table3 %>% filter(method == "ER", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/4)
er.table3 %>% filter(method == "vanilla", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/4)
er.table3 %>% filter(method == "MNPO", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/4)


# ggarrange(plot1.logistic, plot2.logistic, plot3.logistic, plot4.logistic, nrow = 1, common.legend = TRUE, legend = "bottom")

## LDA
er.table3 <- data.frame(method = NA, class = NA, n = NA, value = NA)
n.list <- seq(1000, 10000, 1000)
n.plot <- seq(1000, 10000, 2000)
time.list.avg.lda <- matrix(0, nrow = 10, ncol = 4, dimnames = list(seq(1000, 10000, 1000), c("CX", "ER", "vanilla", "MNPO")))
for (i in 1:500) {
  load(paste0(relative_path, "/output/simulation/lda/", i, ".RData"))
  for (j in 1:10) {
    er.table3 <- rbind(er.table3, data.frame(method = rep(c("CX", "ER", "vanilla", "MNPO"), 4), class = rep(colnames(error3[[j]]), each = 4),n = n.list[[j]], value = as.vector(error3[[j]])))
  }
  time.list.avg.lda <- time.list.avg.lda + time.list3
}
time.list.avg.lda <- time.list.avg.lda/500
er.table3 <- er.table3[-1,]
er.table3$n <- as.factor(er.table3$n)
plot1.lda <- er.table3 %>% filter(method == "CX", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("GNPMC-CX-LDA") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "gray43"),
                     labels = c(TeX(r"($P(\phi(X) = 2|Y=1)$)"), TeX(r"($P(\phi(X) = 1|Y=3)$)"), TeX(r"($P(\phi(X) = 2|Y=3)$)"), "objective value")) +
  geom_hline(yintercept = 0.05, col = "#E69F00") + geom_hline(yintercept = 0.01, col = "#56B4E9") + geom_hline(yintercept = 0.1, col = "#009E73") +
  labs(color='error')
plot2.lda <- er.table3 %>% filter(method == "ER", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("GNPMC-ER-LDA") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "gray43"),
                     labels = c(TeX(r"($P(\phi(X) = 2|Y=1)$)"), TeX(r"($P(\phi(X) = 1|Y=3)$)"), TeX(r"($P(\phi(X) = 2|Y=3)$)"), "objective value")) +
  geom_hline(yintercept = 0.05, col = "#E69F00") + geom_hline(yintercept = 0.01, col = "#56B4E9") + geom_hline(yintercept = 0.1, col = "#009E73") +
  labs(color='error')
plot3.lda <- er.table3 %>% filter(method == "vanilla", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("vanilla-LDA") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "gray43"),
                     labels = c(TeX(r"($P(\phi(X) = 2|Y=1)$)"), TeX(r"($P(\phi(X) = 1|Y=3)$)"), TeX(r"($P(\phi(X) = 2|Y=3)$)"), "objective value")) +
  geom_hline(yintercept = 0.05, col = "#E69F00") + geom_hline(yintercept = 0.01, col = "#56B4E9") + geom_hline(yintercept = 0.1, col = "#009E73") +
  labs(color='error')
plot4.lda <- er.table3 %>% filter(method == "MNPO", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("MNPO-LDA") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "gray43"),
                     labels = c(TeX(r"($P(\phi(X) = 2|Y=1)$)"), TeX(r"($P(\phi(X) = 1|Y=3)$)"), TeX(r"($P(\phi(X) = 2|Y=3)$)"), "objective value")) +
  geom_hline(yintercept = 0.05, col = "#E69F00") + geom_hline(yintercept = 0.01, col = "#56B4E9") + geom_hline(yintercept = 0.1, col = "#009E73") +
  labs(color='error')


er.table3 %>% filter(method == "CX", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/4)
er.table3 %>% filter(method == "ER", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/4)
er.table3 %>% filter(method == "vanilla", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/4)
er.table3 %>% filter(method == "MNPO", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/4)


# ggarrange(plot1.lda, plot2.lda, plot3.lda, plot4.lda, nrow = 1, common.legend = TRUE, legend = "bottom")

## knn
er.table3 <- data.frame(method = NA, class = NA, n = NA, value = NA)
n.list <- seq(1000, 10000, 1000)
n.plot <- seq(1000, 10000, 2000)
time.list.avg.knn <- matrix(0, nrow = 10, ncol = 4, dimnames = list(seq(1000, 10000, 1000), c("CX", "ER", "vanilla", "MNPO")))
for (i in 1:500) {
  load(paste0(relative_path, "/output/simulation/knn/", i, ".RData"))
  for (j in 1:10) {
    er.table3 <- rbind(er.table3, data.frame(method = rep(c("CX", "ER", "vanilla", "MNPO"), 4), class = rep(colnames(error3[[j]]), each = 4),n = n.list[[j]], value = as.vector(error3[[j]])))
  }
  time.list.avg.knn <- time.list.avg.knn + time.list3
}
time.list.avg.knn <- time.list.avg.knn/500
er.table3 <- er.table3[-1,]
er.table3$n <- as.factor(er.table3$n)
plot1.knn <- er.table3 %>% filter(method == "CX", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("GNPMC-CX-kNN") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "gray43"),
                     labels = c(TeX(r"($P(\phi(X) = 2|Y=1)$)"), TeX(r"($P(\phi(X) = 1|Y=3)$)"), TeX(r"($P(\phi(X) = 2|Y=3)$)"), "objective value")) +
  geom_hline(yintercept = 0.05, col = "#E69F00") + geom_hline(yintercept = 0.01, col = "#56B4E9") + geom_hline(yintercept = 0.1, col = "#009E73") +
  labs(color='error')
plot2.knn <- er.table3 %>% filter(method == "ER", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("GNPMC-ER-kNN") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "gray43"),
                     labels = c(TeX(r"($P(\phi(X) = 2|Y=1)$)"), TeX(r"($P(\phi(X) = 1|Y=3)$)"), TeX(r"($P(\phi(X) = 2|Y=3)$)"), "objective value")) +
  geom_hline(yintercept = 0.05, col = "#E69F00") + geom_hline(yintercept = 0.01, col = "#56B4E9") + geom_hline(yintercept = 0.1, col = "#009E73") +
  annotate("text", x = 1:5, y = -0.103, size = 2.8, label = c("(393)", "(170)", "(73)", "(17)", "(4)")) + coord_cartesian(ylim = c(0, 0.55), clip = "off") +
  labs(color='error')
plot3.knn <- er.table3 %>% filter(method == "vanilla", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("vanilla-kNN") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "gray43"),
                     labels = c(TeX(r"($P(\phi(X) = 2|Y=1)$)"), TeX(r"($P(\phi(X) = 1|Y=3)$)"), TeX(r"($P(\phi(X) = 2|Y=3)$)"), "objective value")) +
  geom_hline(yintercept = 0.05, col = "#E69F00") + geom_hline(yintercept = 0.01, col = "#56B4E9") + geom_hline(yintercept = 0.1, col = "#009E73") +
  labs(color='error')
plot4.knn <- er.table3 %>% filter(method == "MNPO", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("MNPO-kNN") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "gray43"),
                     labels = c(TeX(r"($P(\phi(X) = 2|Y=1)$)"), TeX(r"($P(\phi(X) = 1|Y=3)$)"), TeX(r"($P(\phi(X) = 2|Y=3)$)"), "objective value")) +
  geom_hline(yintercept = 0.05, col = "#E69F00") + geom_hline(yintercept = 0.01, col = "#56B4E9") + geom_hline(yintercept = 0.1, col = "#009E73") +
  annotate("text", x = 1:4, y = -0.103, size = 2.8, label = c("(327)", "(60)", "(15)", "(4)")) + coord_cartesian(ylim = c(0, 0.6), clip = "off") +
  labs(color='error')


er.table3 %>% filter(method == "CX", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/4)
er.table3 %>% filter(method == "ER", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/4)
er.table3 %>% filter(method == "vanilla", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/4)
er.table3 %>% filter(method == "MNPO", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/4)

# ggarrange(plot1.knn, plot2.knn, plot3.knn, plot4.knn, nrow = 1, common.legend = TRUE, legend = "bottom")


## nnb
er.table3 <- data.frame(method = NA, class = NA, n = NA, value = NA)
n.list <- seq(1000, 10000, 1000)
n.plot <- seq(1000, 10000, 2000)
time.list.avg.knn <- matrix(0, nrow = 10, ncol = 4, dimnames = list(seq(1000, 10000, 1000), c("CX", "ER", "vanilla", "MNPO")))
for (i in 1:500) {
  load(paste0(relative_path, "/output/simulation/nnb/", i, ".RData"))
  for (j in 1:10) {
    er.table3 <- rbind(er.table3, data.frame(method = rep(c("CX", "ER", "vanilla", "MNPO"), 4), class = rep(colnames(error3[[j]]), each = 4),n = n.list[[j]], value = as.vector(error3[[j]])))
  }
  time.list.avg.nnb <- time.list.avg.nnb + time.list3
}
time.list.avg.nnb <- time.list.avg.nnb/500
er.table3 <- er.table3[-1,]
er.table3$n <- as.factor(er.table3$n)
plot1.nnb <- er.table3 %>% filter(method == "CX", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("GNPMC-CX-NNB") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "gray43"),
                     labels = c(TeX(r"($P(\phi(X) = 2|Y=1)$)"), TeX(r"($P(\phi(X) = 1|Y=3)$)"), TeX(r"($P(\phi(X) = 2|Y=3)$)"), "objective value")) +
  geom_hline(yintercept = 0.05, col = "#E69F00") + geom_hline(yintercept = 0.01, col = "#56B4E9") + geom_hline(yintercept = 0.1, col = "#009E73") +
  labs(color='error')
plot2.nnb <- er.table3 %>% filter(method == "ER", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("GNPMC-ER-NNB") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "gray43"),
                     labels = c(TeX(r"($P(\phi(X) = 2|Y=1)$)"), TeX(r"($P(\phi(X) = 1|Y=3)$)"), TeX(r"($P(\phi(X) = 2|Y=3)$)"), "objective value")) +
  geom_hline(yintercept = 0.05, col = "#E69F00") + geom_hline(yintercept = 0.01, col = "#56B4E9") + geom_hline(yintercept = 0.1, col = "#009E73") +
  annotate("text", x = 1, y = -0.103, size = 2.8, label = c("(1)")) + coord_cartesian(ylim = c(0, 0.55), clip = "off") +
  labs(color='error')
plot3.nnb <- er.table3 %>% filter(method == "vanilla", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("vanilla-NNB") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "gray43"),
                     labels = c(TeX(r"($P(\phi(X) = 2|Y=1)$)"), TeX(r"($P(\phi(X) = 1|Y=3)$)"), TeX(r"($P(\phi(X) = 2|Y=3)$)"), "objective value")) +
  geom_hline(yintercept = 0.05, col = "#E69F00") + geom_hline(yintercept = 0.01, col = "#56B4E9") + geom_hline(yintercept = 0.1, col = "#009E73") +
  labs(color='error')
plot4.nnb <- er.table3 %>% filter(method == "MNPO", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("MNPO-NNB") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "gray43"),
                     labels = c(TeX(r"($P(\phi(X) = 2|Y=1)$)"), TeX(r"($P(\phi(X) = 1|Y=3)$)"), TeX(r"($P(\phi(X) = 2|Y=3)$)"), "objective value")) +
  geom_hline(yintercept = 0.05, col = "#E69F00") + geom_hline(yintercept = 0.01, col = "#56B4E9") + geom_hline(yintercept = 0.1, col = "#009E73") +
  labs(color='error')


er.table3 %>% filter(method == "CX", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/4)
er.table3 %>% filter(method == "ER", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/4)
er.table3 %>% filter(method == "vanilla", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/4)
er.table3 %>% filter(method == "MNPO", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/4)


# ggarrange(plot1.nnb, plot2.nnb, plot3.nnb, plot4.nnb, nrow = 1, common.legend = TRUE, legend = "bottom")


## output: 13 times 11 PDF --- Figure 11
ggarrange(plot1.logistic, plot2.logistic, plot3.logistic, plot4.logistic,
          plot1.lda, plot2.lda, plot3.lda, plot4.lda,
          plot1.knn, plot2.knn, plot3.knn, plot4.knn,
          plot1.nnb, plot2.nnb, plot3.nnb, plot4.nnb, nrow = 4, ncol = 4, common.legend = TRUE, legend = "bottom")

# ------------------------------------
# model 4: an imbalanced version of model 1, "case 3" in the paper
# ------------------------------------
## logistic
er.table4 <- data.frame(method = NA, class = NA, n = NA, value = NA)
n.list <- seq(1000, 10000, 1000)
n.plot <- seq(1000, 10000, 2000)
for (i in 1:500) {
  load(paste0(relative_path, "/output/simulation/logistic/", i, ".RData"))
  for (j in 1:10) {
    er.table4 <- rbind(er.table4, data.frame(method = rep(c("CX", "ER", "vanilla", "MNPO"), 3), class = rep(colnames(error4[[j]]), each = 4),n = n.list[[j]], value = as.vector(error4[[j]])))
  }
}
time.list.avg.logistic <- time.list.avg.logistic/500
er.table4 <- er.table4[-1,]
er.table4$n <- as.factor(er.table4$n)
plot1.logistic <- er.table4 %>% filter(method == "CX", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("NPMC-CX-logistic") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.15, col = "#E69F00") + geom_hline(yintercept = 0.3, col = "#56B4E9") + labs(color='error')
plot2.logistic <- er.table4 %>% filter(method == "ER", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("NPMC-ER-logistic") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.15, col = "#E69F00") + geom_hline(yintercept = 0.3, col = "#56B4E9")  + coord_cartesian(ylim = c(0, 1), clip = "off")+
  annotate("text", x = 1:2, y = -0.173, size = 2.8, label = c("(55)", "(1)")) + labs(color='error')
plot3.logistic <- er.table4 %>% filter(method == "vanilla", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("vanilla-logistic") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.15, col = "#E69F00") + geom_hline(yintercept = 0.3, col = "#56B4E9") + labs(color='error')
plot4.logistic <- er.table4 %>% filter(method == "MNPO", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("MNPO-logistic") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) + coord_cartesian(ylim = c(0, 1), clip = "off")+
  geom_hline(yintercept = 0.15, col = "#E69F00") + geom_hline(yintercept = 0.3, col = "#56B4E9") +
  annotate("text", x = 1:2, y = -0.173, size = 2.8, label = c("(51)", "(3)")) + labs(color='error')

er.table4 %>% filter(method == "CX", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/3)
er.table4 %>% filter(method == "ER", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/3)
er.table4 %>% filter(method == "vanilla", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/3)
er.table4 %>% filter(method == "MNPO", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/3)


# ggarrange(plot1.logistic, plot2.logistic, plot3.logistic, plot4.logistic, nrow = 1, common.legend = TRUE, legend = "bottom")

## LDA
er.table4 <- data.frame(method = NA, class = NA, n = NA, value = NA)
n.list <- seq(1000, 10000, 1000)
n.plot <- seq(1000, 10000, 2000)
for (i in 1:500) {
  load(paste0(relative_path, "/output/simulation/lda/", i, ".RData"))
  for (j in 1:10) {
    er.table4 <- rbind(er.table4, data.frame(method = rep(c("CX", "ER", "vanilla", "MNPO"), 3), class = rep(colnames(error4[[j]]), each = 4),n = n.list[[j]], value = as.vector(error4[[j]])))
  }
}
time.list.avg.lda <- time.list.avg.lda/500
er.table4 <- er.table4[-1,]
er.table4$n <- as.factor(er.table4$n)
plot1.lda <- er.table4 %>% filter(method == "CX", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("NPMC-CX-LDA") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.15, col = "#E69F00") + geom_hline(yintercept = 0.3, col = "#56B4E9") + labs(color='error')
plot2.lda <- er.table4 %>% filter(method == "ER", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("NPMC-ER-LDA") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.15, col = "#E69F00") + geom_hline(yintercept = 0.3, col = "#56B4E9") +
  annotate("text", x = c(1,3), y = -0.17, size = 2.8, label = c("(59)", "(1)")) + coord_cartesian(ylim = c(0, 1), clip = "off") + labs(color='error')
plot3.lda <- er.table4 %>% filter(method == "vanilla", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("vanilla-LDA") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.15, col = "#E69F00") + geom_hline(yintercept = 0.3, col = "#56B4E9") + labs(color='error')
plot4.lda <- er.table4 %>% filter(method == "MNPO", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("MNPO-LDA") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.15, col = "#E69F00") + geom_hline(yintercept = 0.3, col = "#56B4E9") +
  annotate("text", x = 1:3, y = -0.17, size = 2.8, label = c("(38)", "(3)", "(1)")) + coord_cartesian(ylim = c(0, 1), clip = "off") + labs(color='error')


er.table4 %>% filter(method == "CX", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/3)
er.table4 %>% filter(method == "ER", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/3)
er.table4 %>% filter(method == "vanilla", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/3)
er.table4 %>% filter(method == "MNPO", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/3)

# ggarrange(plot1.lda, plot2.lda, plot3.lda, plot4.lda, nrow = 1, common.legend = TRUE, legend = "bottom")

## knn
er.table4 <- data.frame(method = NA, class = NA, n = NA, value = NA)
n.list <- seq(1000, 10000, 1000)
n.plot <- seq(1000, 10000, 2000)
for (i in 1:500) {
  load(paste0(relative_path, "/output/simulation/knn/", i, ".RData"))
  for (j in 1:10) {
    er.table4 <- rbind(er.table4, data.frame(method = rep(c("CX", "ER", "vanilla", "MNPO"), 3), class = rep(colnames(error4[[j]]), each = 4),n = n.list[[j]], value = as.vector(error4[[j]])))
  }
}
er.table4 <- er.table4[-1,]
er.table4$n <- as.factor(er.table4$n)
plot1.knn <- er.table4 %>% filter(method == "CX", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("NPMC-CX-kNN") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.15, col = "#E69F00") + geom_hline(yintercept = 0.3, col = "#56B4E9") +
  annotate("text", x = 1:5, y = -0.125, size = 2.8, label = c("(342)", "(381)", "(393)", "(390)", "(372)")) +
  coord_cartesian(ylim = c(0, 0.7), clip = "off") + labs(color='error')
plot2.knn <- er.table4 %>% filter(method == "ER", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("NPMC-ER-kNN") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.15, col = "#E69F00") + geom_hline(yintercept = 0.3, col = "#56B4E9") +
  annotate("text", x = 1:5, y = -0.125, size = 2.8, label = c("(394)", "(201)", "(69)", "(22)", "(8)")) +
  coord_cartesian(ylim = c(0, 0.7), clip = "off") + labs(color='error')
plot3.knn <- er.table4 %>% filter(method == "vanilla", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("vanilla-kNN") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.15, col = "#E69F00") + geom_hline(yintercept = 0.3, col = "#56B4E9") + labs(color='error')
plot4.knn <- er.table4 %>% filter(method == "MNPO", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("MNPO-kNN") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.15, col = "#E69F00") + geom_hline(yintercept = 0.3, col = "#56B4E9") +
  annotate("text", x = 1:5, y = -0.175, size = 2.8, label = c("(362)", "(143)", "(55)", "(19)", "(3)")) +
  coord_cartesian(ylim = c(0, 1), clip = "off") + labs(color='error')

er.table4 %>% filter(method == "CX", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/3)
er.table4 %>% filter(method == "ER", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/3)
er.table4 %>% filter(method == "vanilla", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/3)
er.table4 %>% filter(method == "MNPO", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/3)

# ggarrange(plot1.knn, plot2.knn, plot3.knn, plot4.knn, nrow = 1, common.legend = TRUE, legend = "bottom")


## nnb
er.table4 <- data.frame(method = NA, class = NA, n = NA, value = NA)
n.list <- seq(1000, 10000, 1000)
n.plot <- seq(1000, 10000, 2000)
for (i in 1:500) {
  load(paste0(relative_path, "/output/simulation/nnb/", i, ".RData"))
  for (j in 1:10) {
    er.table4 <- rbind(er.table4, data.frame(method = rep(c("CX", "ER", "vanilla", "MNPO"), 3), class = rep(colnames(error4[[j]]), each = 4),n = n.list[[j]], value = as.vector(error4[[j]])))
  }
}
er.table4 <- er.table4[-1,]
er.table4$n <- as.factor(er.table4$n)
plot1.nnb <- er.table4 %>% filter(method == "CX", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color = class)) +
  ggtitle("NPMC-CX-NNB") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.15, col = "#E69F00") + geom_hline(yintercept = 0.3, col = "#56B4E9") +
  annotate("text", x = 1, y = -0.17, size = 2.8, label = c("(11)")) + coord_cartesian(ylim = c(0, 1), clip = "off") + labs(color='error')
plot2.nnb <- er.table4 %>% filter(method == "ER", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("NPMC-ER-NNB") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.15, col = "#E69F00") + geom_hline(yintercept = 0.3, col = "#56B4E9") +
  annotate("text", x = 1:3, y = -0.17, size = 2.8, label = c("(115)", "(2)", "(1)")) +
  coord_cartesian(ylim = c(0, 1), clip = "off") + labs(color='error')
plot3.nnb <- er.table4 %>% filter(method == "vanilla", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("vanilla-NNB") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8))  +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.15, col = "#E69F00") + geom_hline(yintercept = 0.3, col = "#56B4E9") + labs(color='error')
plot4.nnb <- er.table4 %>% filter(method == "MNPO", n %in% n.plot)  %>% ggplot(aes(x=n, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("MNPO-NNB") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8))  +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.15, col = "#E69F00") + geom_hline(yintercept = 0.3, col = "#56B4E9") +
  annotate("text", x = 1:2, y = -0.17, size = 2.8, label = c("(108)", "(9)")) +
  coord_cartesian(ylim = c(0, 1), clip = "off") + labs(color='error')

er.table4 %>% filter(method == "CX", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/3)
er.table4 %>% filter(method == "ER", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/3)
er.table4 %>% filter(method == "vanilla", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/3)
er.table4 %>% filter(method == "MNPO", n %in% n.plot) %>% group_by(n) %>% summarise(na_count = sum(is.na(value))/3)

# ggarrange(plot1.nnb, plot2.nnb, plot3.nnb, plot4.nnb, nrow = 1, common.legend = TRUE, legend = "bottom")

## output: 13 times 11 PDF: model4.pdf  --- Figure 10
ggarrange(plot1.logistic, plot2.logistic, plot3.logistic, plot4.logistic,
          plot1.lda, plot2.lda, plot3.lda, plot4.lda,
          plot1.knn, plot2.knn, plot3.knn, plot4.knn,
          plot1.nnb, plot2.nnb, plot3.nnb, plot4.nnb, nrow = 4, ncol = 4, common.legend = TRUE, legend = "bottom")


# -------------------------------------
# real_data: beans
# -------------------------------------

er.table <- data.frame(method = NA, class = NA, classifier = NA, value = NA)
for (i in 1:500) {
  load(paste0(relative_path, "/output/real_data/beans/", i, ".RData"))
  for (j in 1:4) {
    error[[j]] <- cbind(error[[j]], objective = error[[j]][, 3]/4 + error[[j]][, 5]/4 + error[[j]][, 6]/4 + error[[j]][, 7]/4)
    er.table <- rbind(er.table, data.frame(method = rep(c("NPMC-CX", "NPMC-ER", "vanilla"), 8), class = rep(colnames(error[[j]]), each = 3), classifier = names(error)[j], value = as.vector(error[[j]])))
  }
}
er.table <- er.table[-1,]
er.table$classifier <- as.factor(er.table$classifier)

plot1 <- er.table %>% filter(classifier == "logistic")  %>% ggplot(aes(x=method, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("logistic") + theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "mediumpurple2", "gold1", "orchid1", "#0072B2", "gray43"),
                     labels = c(paste("class", 1:7), "objective value")) +
  geom_hline(yintercept = 0.05, col = "#E69F00") + geom_hline(yintercept = 0.01, col = "#56B4E9") +
  geom_hline(yintercept = 0.03, col = "mediumpurple2") + labs(color='error')

er.table %>% filter(classifier == "logistic", method == "NPMC-CX") %>% summarise(na_count = sum(is.na(value))/8)
er.table %>% filter(classifier == "logistic", method == "NPMC-ER") %>% summarise(na_count = sum(is.na(value))/8)

plot2 <- er.table %>% filter(classifier == "knn")  %>% ggplot(aes(x=method, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("kNN") + theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "mediumpurple2", "gold1", "orchid1", "#0072B2", "gray43"),
                     labels = c(paste("class", 1:7), "objective value")) +
  geom_hline(yintercept = 0.05, col = "#E69F00") + geom_hline(yintercept = 0.01, col = "#56B4E9") +
  geom_hline(yintercept = 0.03, col = "mediumpurple2") + labs(color='error')

er.table %>% filter(classifier == "knn", method == "NPMC-CX") %>% summarise(na_count = sum(is.na(value))/8)
er.table %>% filter(classifier == "knn", method == "NPMC-ER") %>% summarise(na_count = sum(is.na(value))/8)

plot3 <- er.table %>% filter(classifier == "svm")  %>% ggplot(aes(x=method, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("SVM") + theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "mediumpurple2", "gold1", "orchid1", "#0072B2", "gray43"),
                     labels = c(paste("class", 1:7), "objective value")) +
  geom_hline(yintercept = 0.05, col = "#E69F00") + geom_hline(yintercept = 0.01, col = "#56B4E9") +
  geom_hline(yintercept = 0.03, col = "mediumpurple2") + labs(color='error')

er.table %>% filter(classifier == "svm", method == "NPMC-CX") %>% summarise(na_count = sum(is.na(value))/8)
er.table %>% filter(classifier == "svm", method == "NPMC-ER") %>% summarise(na_count = sum(is.na(value))/8)


plot4 <- er.table %>% filter(classifier == "randomforest")  %>% ggplot(aes(x=method, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("RF") + theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "mediumpurple2", "gold1", "orchid1", "#0072B2", "gray43"),
                     labels = c(paste("class", 1:7), "objective value")) +
  geom_hline(yintercept = 0.05, col = "#E69F00") + geom_hline(yintercept = 0.01, col = "#56B4E9") +
  geom_hline(yintercept = 0.03, col = "mediumpurple2") + labs(color='error')

er.table %>% filter(classifier == "randomforest", method == "NPMC-CX") %>% summarise(na_count = sum(is.na(value))/8)
er.table %>% filter(classifier == "randomforest", method == "NPMC-ER") %>% summarise(na_count = sum(is.na(value))/8)

# output: 12 times 9 PDF  --- Figure 15
ggarrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")

# -------------------------------------
# real_data: sat
# -------------------------------------
er.table <- data.frame(method = NA, class = NA, classifier = NA, value = NA)
for (i in 1:500) {
  load(paste0(relative_path, "/output/real_data/sat/", i, ".RData"))
  for (j in 1:4) {
    error[[j]] <- cbind(error[[j]], objective = rowMeans(error[[j]]))
    er.table <- rbind(er.table, data.frame(method = rep(c("NPMC-CX", "NPMC-ER", "vanilla"), 7), class = rep(colnames(error[[j]]), each = 3), classifier = names(error)[j], value = as.vector(error[[j]])))
  }
}
er.table <- er.table[-1,]
er.table$classifier <- as.factor(er.table$classifier)



plot1 <- er.table %>% filter(classifier == "logistic")  %>% ggplot(aes(x=method, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("logistic") + theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "mediumpurple2", "gold1", "orchid1", "gray43"),
                     labels = c(paste("class", 1:6), "objective value")) +
  geom_hline(yintercept = 0.15, col = "#009E73") + geom_hline(yintercept = 0.2, col = "mediumpurple2") +
  geom_hline(yintercept = 0.1, col = "gold1") + labs(color='error')

er.table %>% filter(classifier == "logistic", method == "NPMC-CX") %>% summarise(na_count = sum(is.na(value))/7)
er.table %>% filter(classifier == "logistic", method == "NPMC-ER") %>% summarise(na_count = sum(is.na(value))/7)


plot2 <- er.table %>% filter(classifier == "knn")  %>% ggplot(aes(x=method, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("kNN") + theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "mediumpurple2", "gold1", "orchid1", "gray43"),
                     labels = c(paste("class", 1:6), "objective value")) +
  geom_hline(yintercept = 0.15, col = "#009E73") + geom_hline(yintercept = 0.2, col = "mediumpurple2") +
  geom_hline(yintercept = 0.1, col = "gold1") + labs(color='error')

er.table %>% filter(classifier == "knn", method == "NPMC-CX") %>% summarise(na_count = sum(is.na(value))/7)
er.table %>% filter(classifier == "knn", method == "NPMC-ER") %>% summarise(na_count = sum(is.na(value))/7)



plot3 <- er.table %>% filter(classifier == "svm")  %>% ggplot(aes(x=method, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("SVM") + theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "mediumpurple2", "gold1", "orchid1", "gray43"),
                     labels = c(paste("class", 1:6), "objective value")) +
  geom_hline(yintercept = 0.15, col = "#009E73") + geom_hline(yintercept = 0.2, col = "mediumpurple2") +
  geom_hline(yintercept = 0.1, col = "gold1") + labs(color='error')

er.table %>% filter(classifier == "svm", method == "NPMC-CX") %>% summarise(na_count = sum(is.na(value))/7)
er.table %>% filter(classifier == "svm", method == "NPMC-ER") %>% summarise(na_count = sum(is.na(value))/7)


plot4 <- er.table %>% filter(classifier == "randomforest")  %>% ggplot(aes(x=method, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("RF") + theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "mediumpurple2", "gold1", "orchid1", "gray43"),
                     labels = c(paste("class", 1:6), "objective value")) +
  geom_hline(yintercept = 0.15, col = "#009E73") + geom_hline(yintercept = 0.2, col = "mediumpurple2") +
  geom_hline(yintercept = 0.1, col = "gold1") + labs(color='error')

er.table %>% filter(classifier == "randomforest", method == "NPMC-CX") %>% summarise(na_count = sum(is.na(value))/7)
er.table %>% filter(classifier == "randomforest", method == "NPMC-ER") %>% summarise(na_count = sum(is.na(value))/7)

# output: 12 times 9 PDF  --- Figure 16
ggarrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")



# -------------------------------------
# real_data: dementia
# -------------------------------------

# without smote
er.table <- data.frame(method = NA, class = NA, classifier = NA, value = NA)
for (i in 1:500) {
  load(paste0(relative_path, "/output/real_data/dementia/", i, ".RData"))
  for (j in 1:4) {
    er.table <- rbind(er.table, data.frame(method = rep(c("NPMC-CX", "NPMC-ER", "vanilla"), 3), class = rep(colnames(error[[j]]), each = 3), classifier = names(error)[j], value = as.vector(error[[j]])))
  }
}
er.table <- er.table[-1,]
er.table$classifier <- as.factor(er.table$classifier)

plot1 <- er.table %>% filter(classifier == "logistic")  %>% ggplot(aes(x=method, y=value)) + geom_boxplot(aes(color = class)) +
  ggtitle("logistic") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.1, col = "#E69F00") + geom_hline(yintercept = 0.02, col = "#56B4E9") +
  annotate("text", x = 2, y = -0.12, size = 2.8, label = c("(374)")) + coord_cartesian(ylim = c(0, 1), clip = "off") + labs(color='error')

er.table %>% filter(classifier == "logistic", method == "NPMC-CX") %>% summarise(na_count = sum(is.na(value))/3)
er.table %>% filter(classifier == "logistic", method == "NPMC-ER") %>% summarise(na_count = sum(is.na(value))/3)



plot2 <- er.table %>% filter(classifier == "knn")  %>% ggplot(aes(x=method, y=value)) + geom_boxplot(aes(color = class)) +
  ggtitle("kNN") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.1, col = "#E69F00") + geom_hline(yintercept = 0.02, col = "#56B4E9") +
  annotate("text", x = 1:2, y = -0.096, size = 2.8, label = c("(117)", "(128)")) + coord_cartesian(ylim = c(0, 0.8), clip = "off") + labs(color='error')

er.table %>% filter(classifier == "knn", method == "NPMC-CX") %>% summarise(na_count = sum(is.na(value))/3)
er.table %>% filter(classifier == "knn", method == "NPMC-ER") %>% summarise(na_count = sum(is.na(value))/3)



plot3 <- er.table %>% filter(classifier == "svm")  %>% ggplot(aes(x=method, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("SVM") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.1, col = "#E69F00") + geom_hline(yintercept = 0.02, col = "#56B4E9") +
  annotate("text", x = 1:2, y = -0.12, size = 2.8, label = c("(38)", "(3)")) +
  coord_cartesian(ylim = c(0, 1), clip = "off") + labs(color='error')


er.table %>% filter(classifier == "svm", method == "NPMC-CX") %>% summarise(na_count = sum(is.na(value))/3)
er.table %>% filter(classifier == "svm", method == "NPMC-ER") %>% summarise(na_count = sum(is.na(value))/3)


plot4 <- er.table %>% filter(classifier == "randomforest")  %>% ggplot(aes(x=method, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("RF") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.1, col = "#E69F00") + geom_hline(yintercept = 0.02, col = "#56B4E9") + labs(color='error')

er.table %>% filter(classifier == "randomforest", method == "NPMC-CX") %>% summarise(na_count = sum(is.na(value))/3)
er.table %>% filter(classifier == "randomforest", method == "NPMC-ER") %>% summarise(na_count = sum(is.na(value))/3)

# output: 12 times 9 PDF  --- Figure 17
ggarrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")



# with smote
er.table <- data.frame(method = NA, class = NA, classifier = NA, value = NA)
for (i in 1:500) {
  load(paste0(relative_path, "/output/real_data/dementia/", i, ".RData"))
  for (j in 1:4) {
    er.table <- rbind(er.table, data.frame(method = rep(c("NPMC-CX", "NPMC-ER", "vanilla"), 3), class = rep(colnames(error.smote[[j]]), each = 3), classifier = names(error.smote)[j], value = as.vector(error.smote[[j]])))
  }
}
er.table <- er.table[-1,]
er.table$classifier <- as.factor(er.table$classifier)

plot1 <- er.table %>% filter(classifier == "logistic")  %>% ggplot(aes(x=method, y=value)) + geom_boxplot(aes(color = class)) +
  ggtitle("logistic") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.1, col = "#E69F00") + geom_hline(yintercept = 0.02, col = "#56B4E9") +
  annotate("text", x = 2, y = -0.12, size = 2.8, label = c("(10)")) + coord_cartesian(ylim = c(0, 1), clip = "off") + labs(color='error')

er.table %>% filter(classifier == "logistic", method == "NPMC-CX") %>% summarise(na_count = sum(is.na(value))/3)
er.table %>% filter(classifier == "logistic", method == "NPMC-ER") %>% summarise(na_count = sum(is.na(value))/3)



plot2 <- er.table %>% filter(classifier == "knn")  %>% ggplot(aes(x=method, y=value)) + geom_boxplot(aes(color = class)) +
  ggtitle("kNN") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.1, col = "#E69F00") + geom_hline(yintercept = 0.02, col = "#56B4E9") +
  annotate("text", x = 1:2, y = -0.123, size = 2.8, label = c("(97)", "(39)")) + coord_cartesian(ylim = c(0, 1), clip = "off") + labs(color='error')

er.table %>% filter(classifier == "knn", method == "NPMC-CX") %>% summarise(na_count = sum(is.na(value))/3)
er.table %>% filter(classifier == "knn", method == "NPMC-ER") %>% summarise(na_count = sum(is.na(value))/3)



plot3 <- er.table %>% filter(classifier == "svm")  %>% ggplot(aes(x=method, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("SVM") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.1, col = "#E69F00") + geom_hline(yintercept = 0.02, col = "#56B4E9") +
  annotate("text", x = 2, y = -0.12, size = 2.8, label = c("(2)")) +
  coord_cartesian(ylim = c(0, 1), clip = "off") + labs(color='error')


er.table %>% filter(classifier == "svm", method == "NPMC-CX") %>% summarise(na_count = sum(is.na(value))/3)
er.table %>% filter(classifier == "svm", method == "NPMC-ER") %>% summarise(na_count = sum(is.na(value))/3)


plot4 <- er.table %>% filter(classifier == "randomforest")  %>% ggplot(aes(x=method, y=value)) + geom_boxplot(aes(color=class)) +
  ggtitle("RF") + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.1, col = "#E69F00") + geom_hline(yintercept = 0.02, col = "#56B4E9") +
  annotate("text", x = 1:2, y = -0.12, size = 2.8, label = c("(12)", "(3)")) +
  coord_cartesian(ylim = c(0, 1), clip = "off") + labs(color='error')

er.table %>% filter(classifier == "randomforest", method == "NPMC-CX") %>% summarise(na_count = sum(is.na(value))/3)
er.table %>% filter(classifier == "randomforest", method == "NPMC-ER") %>% summarise(na_count = sum(is.na(value))/3)

# output: 12 times 9 PDF  --- Figure 18
ggarrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")



# -------------------------
# real data - LendingClub
# -------------------------

# -------------------------
# NPMC problem: alpha1 = 0.3, alpha2 = 0.5

alpha <- c(0.3, 0.5, NA)

er.table <- data.frame(method = NA, class = NA, value = NA)
for (i in 1:500) {
  load(paste0(relative_path, "/output/real_data/loanclub/loanclub/", i, ".RData"))
  er.table <- rbind(er.table, data.frame(method = rep(rownames(error), 3), class = rep(colnames(error), each = 4), value = as.vector(error)))
}
er.table <- er.table[-1,]

er.table$method[er.table$method == "CX-logistic"] <- "NPMC-CX-logistic"
er.table$method[er.table$method == "ER-randomforest"] <- "NPMC-ER-RF"
er.table$method[er.table$method == "randomforest"] <- "RF"
er.table$method <- factor(er.table$method, levels = c("NPMC-CX-logistic", "NPMC-ER-RF", "logistic", "RF"))

# save as a 10*3 pdf - for paper
er.table %>% ggplot(aes(x=method, y=value)) + geom_boxplot(aes(color=class)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.3, col ="#E69F00") + geom_hline(yintercept = 0.5, col = "#56B4E9") +
  coord_cartesian(ylim = c(0, 1), clip = "off") + labs(color = "error")

er.table %>% filter(method == "NPMC-CX-logistic") %>% summarise(na_count = sum(is.na(value))/3)
er.table %>% filter(method == "NPMC-ER-RF") %>% summarise(na_count = sum(is.na(value))/3)

# save as a 10*6 pdf - for slides
er.table %>% ggplot(aes(x=method, y=value)) + geom_boxplot(aes(color=class)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  geom_hline(yintercept = 0.3, col ="#E69F00") + geom_hline(yintercept = 0.5, col = "#56B4E9") +
  coord_cartesian(ylim = c(0, 1), clip = "off") + labs(color = "error")


# -------------------------
# GNPMC problem: alpha13 = 0.1, alpha2 = 0.15

alpha <- c(0.1, 0.15, NA)

er.table <- data.frame(method = NA, class = NA, value = NA)
for (i in 1:500) {
  load(paste0(relative_path, "/output/real_data/loanclub/loanclub/", i, ".RData"))
  er.table <- rbind(er.table, data.frame(method = rep(rownames(error_confusion), 3), class = rep(colnames(error_confusion), each = 4), value = as.vector(error_confusion)))
}
er.table <- er.table[-1,]

er.table$method[er.table$method == "CX-logistic"] <- "GNPMC-CX-logistic"
er.table$method[er.table$method == "ER-randomforest"] <- "GNPMC-ER-RF"
er.table$method[er.table$method == "randomforest"] <- "RF"
er.table$method <- factor(er.table$method, levels = c("GNPMC-CX-logistic", "GNPMC-ER-RF", "logistic", "RF"))

# save as a 10*3 pdf --- Figure 14
er.table %>% ggplot(aes(x=method, y=value)) + geom_boxplot(aes(color=class)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", 'gray43'),
                     labels = c(TeX(r"($P(\phi(X) = 3|Y=1)$)"), TeX(r"($P(\phi(X) = 3|Y=2)$)"), "objective value")) +
  geom_hline(yintercept = 0.1, col = "#E69F00") + geom_hline(yintercept = 0.15, col = "#56B4E9") +
  coord_cartesian(ylim = c(0, 1), clip = "off") + labs(color='error')

er.table %>% filter(method == "GNPMC-CX-logistic") %>% summarise(na_count = sum(is.na(value))/3)
er.table %>% filter(method == "GNPMC-ER-RF") %>% summarise(na_count = sum(is.na(value))/3)



# --------------------------
# logistic

## NPMC
s.matrix <- matrix(nrow = 100, ncol = 100, dimnames = list((1:100)/100, (1:100)/100))
f.matrix <- matrix(nrow = 100, ncol = 100, dimnames = list((1:100)/100, (1:100)/100))
obj.matrix <- matrix(nrow = 100, ncol = 100, dimnames = list((1:100)/100, (1:100)/100))
for (i in 1:100) {
  load(paste0(relative_path, "/output/real_data/loanclub/loanclub_logistic/", i, ".RData"))
  s.matrix[i, ] <- D_matrix[, "s"]
  f.matrix[i, ] <- D_matrix[, "f"]
  obj.matrix[i, ] <- D_matrix[, "obj.value"]
}


data <- expand.grid(alpha1=(1:100)/100, alpha2=(1:100)/100)
data$s <- as.vector(s.matrix)
data$f <- as.vector(f.matrix)
data$obj_value <- as.vector(obj.matrix)
data$s <- factor(data$s)
data[data$f == 0, "obj_value"] <- NA

data[, "combination"] <- NA
data[data$s == 0 & data$f == 0, "combination"] <- "s = 0, f = 0"
data[data$s == 1 & data$f == 0, "combination"] <- "s = 1, f = 0"
data[data$s == 0 & data$f == 1, "combination"] <- "s = 0, f = 1"
data[data$s == 1 & data$f == 1, "combination"] <- "s = 1, f = 1"
data[, "combination"] <- factor(data[, "combination"], levels = c("s = 1, f = 0", "s = 0, f = 0", "s = 0, f = 1", "s = 1, f = 1"))



# Heatmap
plot_left <- ggplot(data, aes(alpha1, alpha2, fill = obj_value)) + geom_tile() +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom",
        legend.margin = margin(t = 48, r = 0, b = 0, l = 0, unit = "pt")) +
  xlab(TeX(r"($\alpha_1$)")) + ylab(TeX(r"($\alpha_2$)")) + labs(fill='objective value') +
  geom_point(aes(size=""), shape =NA, colour = "darkgray")+
  guides(size=guide_legend(title="infeasible", title.vjust = 0.6, override.aes=list(shape=15, size = 7)),
         fill=guide_colorbar(title.vjust = 0.8))

plot_right <- data %>% ggplot(aes(alpha1, alpha2, fill= combination)) + geom_tile() +
  theme(plot.title = element_text(hjust = 0.5), legend.spacing.x = unit(2, "cm"),
        legend.position="bottom") +
  guides(fill = guide_legend(nrow = 4)) + xlab(TeX(r"($\alpha_1$)")) + ylab(TeX(r"($\alpha_2$)")) +
  scale_fill_manual(values= c("s = 0, f = 0" = '#d7191c',
                              "s = 1, f = 0" = '#fdae61',
                              "s = 0, f = 1" = '#abd9e9',
                              "s = 1, f = 1" = '#2c7bb6'),
                    labels=c("", "", "                                                                            ", ""),
                    drop=FALSE) +
  labs(fill='')

# save as a 10*5 pdf  --- Figure 3
ggarrange(plot_left, plot_right, nrow = 1, ncol = 2)

## GNPMC
s.matrix <- matrix(nrow = 100, ncol = 100, dimnames = list((1:100)/100, (1:100)/100))
f.matrix <- matrix(nrow = 100, ncol = 100, dimnames = list((1:100)/100, (1:100)/100))
obj.matrix <- matrix(nrow = 100, ncol = 100, dimnames = list((1:100)/100, (1:100)/100))
for (i in 1:100) {
  load(paste0(relative_path, "/output/real_data/loanclub/loanclub_logistic/", i, ".RData"))
  s.matrix[i, ] <- D_matrix_confusion[, "s"]
  f.matrix[i, ] <- D_matrix_confusion[, "f"]
  obj.matrix[i, ] <- D_matrix_confusion[, "obj.value"]
}


data <- expand.grid(alpha13=(1:100)/100, alpha23=(1:100)/100)
data$s <- as.vector(s.matrix)
data$f <- as.vector(f.matrix)
data$obj_value <- as.vector(obj.matrix)
data$s <- factor(data$s)
data[data$f == 0, "obj_value"] <- NA

data[, "combination"] <- NA
data[data$s == 0 & data$f == 0, "combination"] <- "s = 0, f = 0"
data[data$s == 1 & data$f == 0, "combination"] <- "s = 1, f = 0"
data[data$s == 0 & data$f == 1, "combination"] <- "s = 0, f = 1"
data[data$s == 1 & data$f == 1, "combination"] <- "s = 1, f = 1"
data[, "combination"] <- factor(data[, "combination"], levels = c("s = 1, f = 0", "s = 0, f = 0", "s = 0, f = 1", "s = 1, f = 1"))



# Heatmap
plot_left <- ggplot(data, aes(alpha13, alpha23, fill = obj_value)) + geom_tile() +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom",
        legend.margin = margin(t = 48, r = 0, b = 0, l = 0, unit = "pt")) +
  xlab(TeX(r"($\alpha_{13}$)")) + ylab(TeX(r"($\alpha_{23}$)")) + labs(fill='objective value') +
  geom_point(aes(size=""), shape =NA, colour = "darkgray")+
  guides(size=guide_legend(title="infeasible", title.vjust = 0.6, override.aes=list(shape=15, size = 7)),
         fill=guide_colorbar(title.vjust = 0.8))

plot_right <- data %>% ggplot(aes(alpha13, alpha23, fill= combination)) + geom_tile() +
  theme(plot.title = element_text(hjust = 0.5), legend.spacing.x = unit(2, "cm"),
        legend.position="bottom") +
  guides(fill = guide_legend(nrow = 4)) + xlab(TeX(r"($\alpha_{13}$)")) + ylab(TeX(r"($\alpha_{23}$)")) +
  scale_fill_manual(values= c("s = 0, f = 0" = '#d7191c',
                              "s = 1, f = 0" = '#fdae61',
                              "s = 0, f = 1" = '#abd9e9',
                              "s = 1, f = 1" = '#2c7bb6'),
                    labels=c("", "", "                                                                            ", ""),
                    drop=FALSE) +
  labs(fill='')

# save as a 10*5 pdf  --- Figure 12
ggarrange(plot_left, plot_right, nrow = 1, ncol = 2)


# --------------------------
# RF

## NPMC
s.matrix <- matrix(nrow = 100, ncol = 100, dimnames = list((1:100)/100, (1:100)/100))
f.matrix <- matrix(nrow = 100, ncol = 100, dimnames = list((1:100)/100, (1:100)/100))
obj.matrix <- matrix(nrow = 100, ncol = 100, dimnames = list((1:100)/100, (1:100)/100))
for (i in 1:100) {
  a <- try(load(paste0(relative_path, "/output/real_data/loanclub/loanclub_rf/", i, ".RData")))
  s.matrix[i, ] <- D_matrix[, "s"]
  f.matrix[i, ] <- D_matrix[, "f"]
  obj.matrix[i, ] <- D_matrix[, "obj.value"]
}


data <- expand.grid(alpha1=(1:100)/100, alpha2=(1:100)/100)
data$s <- as.vector(s.matrix)
data$f <- as.vector(f.matrix)
data$obj_value <- as.vector(obj.matrix)
data$s <- factor(data$s)
data[data$f == 0, "obj_value"] <- NA

data[, "combination"] <- NA
data[data$s == 0 & data$f == 0, "combination"] <- "s = 0, f = 0"
data[data$s == 1 & data$f == 0, "combination"] <- "s = 1, f = 0"
data[data$s == 0 & data$f == 1, "combination"] <- "s = 0, f = 1"
data[data$s == 1 & data$f == 1, "combination"] <- "s = 1, f = 1"
data[, "combination"] <- factor(data[, "combination"], levels = c("s = 1, f = 0", "s = 0, f = 0", "s = 0, f = 1", "s = 1, f = 1"))



# Heatmap
plot_left <- ggplot(data, aes(alpha1, alpha2, fill = obj_value)) + geom_tile() +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom",
        legend.margin = margin(t = 48, r = 0, b = 0, l = 0, unit = "pt")) +
  xlab(TeX(r"($\alpha_1$)")) + ylab(TeX(r"($\alpha_2$)")) + labs(fill='objective value') +
  geom_point(aes(size=""), shape =NA, colour = "darkgray")+
  guides(size=guide_legend(title="infeasible", title.vjust = 0.6, override.aes=list(shape=15, size = 7)),
         fill=guide_colorbar(title.vjust = 0.8))


plot_right <- data %>% ggplot(aes(alpha1, alpha2, fill= combination)) + geom_tile() +
  theme(plot.title = element_text(hjust = 0.5), legend.spacing.x = unit(2, "cm"),
        legend.position="bottom") +
  guides(fill = guide_legend(nrow = 4)) + xlab(TeX(r"($\alpha_1$)")) + ylab(TeX(r"($\alpha_2$)")) +
  scale_fill_manual(values= c("s = 0, f = 0" = '#d7191c',
                              "s = 1, f = 0" = '#fdae61',
                              "s = 0, f = 1" = '#abd9e9',
                              "s = 1, f = 1" = '#2c7bb6'),
                    labels=c("", "", "                                                                            ", ""),
                    drop=FALSE) +
  labs(fill='')

# save as a 10*5 pdf  --- Figure 4
ggarrange(plot_left, plot_right, nrow = 1, ncol = 2)


## GNPMC
s.matrix <- matrix(nrow = 100, ncol = 100, dimnames = list((1:100)/100, (1:100)/100))
f.matrix <- matrix(nrow = 100, ncol = 100, dimnames = list((1:100)/100, (1:100)/100))
obj.matrix <- matrix(nrow = 100, ncol = 100, dimnames = list((1:100)/100, (1:100)/100))
for (i in 1:100) {
  load(paste0(relative_path, "/output/real_data/loanclub/loanclub_rf/", i, ".RData"))
  s.matrix[i, ] <- D_matrix_confusion[, "s"]
  f.matrix[i, ] <- D_matrix_confusion[, "f"]
  obj.matrix[i, ] <- D_matrix_confusion[, "obj.value"]
}


data <- expand.grid(alpha13=(1:100)/100, alpha23=(1:100)/100)
data$s <- as.vector(s.matrix)
data$f <- as.vector(f.matrix)
data$obj_value <- as.vector(obj.matrix)
data$s <- factor(data$s)
data[data$f == 0, "obj_value"] <- NA

data[, "combination"] <- NA
data[data$s == 0 & data$f == 0, "combination"] <- "s = 0, f = 0"
data[data$s == 1 & data$f == 0, "combination"] <- "s = 1, f = 0"
data[data$s == 0 & data$f == 1, "combination"] <- "s = 0, f = 1"
data[data$s == 1 & data$f == 1, "combination"] <- "s = 1, f = 1"
data[, "combination"] <- factor(data[, "combination"], levels = c("s = 1, f = 0", "s = 0, f = 0", "s = 0, f = 1", "s = 1, f = 1"))



# Heatmap
plot_left <- ggplot(data, aes(alpha13, alpha23, fill = obj_value)) + geom_tile() +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom",
        legend.margin = margin(t = 48, r = 0, b = 0, l = 0, unit = "pt")) +
  xlab(TeX(r"($\alpha_{13}$)")) + ylab(TeX(r"($\alpha_{23}$)")) + labs(fill='objective value') +
  geom_point(aes(size=""), shape =NA, colour = "darkgray")+
  guides(size=guide_legend(title="infeasible", title.vjust = 0.6, override.aes=list(shape=15, size = 7)),
         fill=guide_colorbar(title.vjust = 0.8))

plot_right <- data %>% ggplot(aes(alpha13, alpha23, fill= combination)) + geom_tile() +
  theme(plot.title = element_text(hjust = 0.5), legend.spacing.x = unit(2, "cm"),
        legend.position="bottom") +
  guides(fill = guide_legend(nrow = 4)) + xlab(TeX(r"($\alpha_{13}$)")) + ylab(TeX(r"($\alpha_{23}$)")) +
  scale_fill_manual(values= c("s = 0, f = 0" = '#d7191c',
                              "s = 1, f = 0" = '#fdae61',
                              "s = 0, f = 1" = '#abd9e9',
                              "s = 1, f = 1" = '#2c7bb6'),
                    labels=c("", "", "                                                                            ", ""),
                    drop=FALSE) +
  labs(fill='')

# save as a 10*5 pdf  --- Figure 13
ggarrange(plot_left, plot_right, nrow = 1, ncol = 2)

