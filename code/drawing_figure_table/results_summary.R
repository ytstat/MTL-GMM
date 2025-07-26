library(latex2exp)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(scales)

source("results_summary_func.R")

# the absolute path of the folder for the stored output results
folder_path <- ""

# -------------------------------------------
# Simulation 1: MTL-1, Figure 2, Figures S.6, and Figure S.7
# -------------------------------------------

er.combined <- load_result(folder_path,
                           experiment_no = "mtl_1")

plot_w <- sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = w_mean, color = method, shape = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{w^{(k)*}\}_{k \in S}$)")) +
    xlab(TeX(r"($h$)")) + ylab("Error") + scale_x_continuous(breaks = h) +
    geom_errorbar(aes(ymin=w_mean-w_sd, ymax=w_mean+w_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")
}, simplify = F)



plot_mu <- sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = mu_mean, color = method, shape = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\mu^{(k)*}_1\}_{k \in S}$ and $\{\mu^{(k)*}_2\}_{k \in S}$)")) +
    xlab(TeX(r"($h$)")) + ylab("Error") + scale_x_continuous(breaks = h) +
    geom_errorbar(aes(ymin=mu_mean-mu_sd, ymax=mu_mean+mu_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")
}, simplify = F)


plot_beta2 <-  sapply(0:2, function(i){
  if(i == 0) {
    er.combined %>% filter(outlier_num ==i) %>%
      ggplot(aes(x = h, y = beta_mean, color = method, shape = method)) + geom_point(size = 3) +
      geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\beta^{(k)*}\}_{k \in S}$ (without outlier tasks))")) +
      xlab(TeX(r"($h$)")) + ylab("Error") + scale_x_continuous(breaks = h) +
      geom_errorbar(aes(ymin=beta_mean-beta_sd, ymax=beta_mean+beta_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")
  } else if (i == 1) {
    er.combined %>% filter(outlier_num ==i) %>%
      ggplot(aes(x = h, y = beta_mean, color = method, shape = method)) + geom_point(size = 3) +
      geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\beta^{(k)*}\}_{k \in S}$ (with 1 outlier task))")) +
      xlab(TeX(r"($h$)")) + ylab("Error") + scale_x_continuous(breaks = h) +
      geom_errorbar(aes(ymin=beta_mean-beta_sd, ymax=beta_mean+beta_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")
  } else if (i == 2) {
    er.combined %>% filter(outlier_num ==i) %>%
      ggplot(aes(x = h, y = beta_mean, color = method, shape = method)) + geom_point(size = 3) +
      geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\beta^{(k)*}\}_{k \in S}$ (with 2 outlier tasks))")) +
      xlab(TeX(r"($h$)")) + ylab("Error") + scale_x_continuous(breaks = h) +
      geom_errorbar(aes(ymin=beta_mean-beta_sd, ymax=beta_mean+beta_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")
  }
}, simplify = F)


plot_delta <-  sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = delta_mean, color = method, shape = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\delta^{(k)*}\}_{k \in S}$)")) +
    xlab(TeX(r"($h$)")) + ylab("Error") + scale_x_continuous(breaks = h) +
    geom_errorbar(aes(ymin=delta_mean-delta_sd, ymax=delta_mean+delta_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")
}, simplify = F)


plot_Sigma <- sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = Sigma_mean, color = method, shape = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\Sigma^{(k)*}\}_{k \in S}$)")) +
    xlab(TeX(r"($h$)")) + ylab("Error")  + scale_x_continuous(breaks = h) +
    geom_errorbar(aes(ymin=Sigma_mean-Sigma_sd, ymax=Sigma_mean+Sigma_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")
}, simplify = F)



plot_misclustering_max <- sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = misclustering_max_mean, color = method, shape = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
    xlab(TeX(r"($h$)")) + ylab(TeX(r"($\max_{k \in S}\hat{R}^{(k)}(\hat{C}^{(k)})$)"))  + scale_x_continuous(breaks = h) +
    geom_errorbar(aes(ymin=misclustering_max_mean-misclustering_max_sd, ymax=misclustering_max_mean+misclustering_max_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")
}, simplify = F)

plot_misclustering_max2 <- sapply(0:2, function(i){
  if (i == 0) {
    er.combined %>% filter(outlier_num ==i) %>%
      ggplot(aes(x = h, y = misclustering_max_mean, color = method, shape = method)) + geom_point(size = 3) +
      geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle("Maximum mis-clustering error (without outlier tasks)") +
      xlab(TeX(r"($h$)")) + ylab("Error")  + scale_x_continuous(breaks = h) +
      geom_errorbar(aes(ymin=misclustering_max_mean-misclustering_max_sd, ymax=misclustering_max_mean+misclustering_max_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")
  } else if (i == 1) {
    er.combined %>% filter(outlier_num ==i) %>%
      ggplot(aes(x = h, y = misclustering_max_mean, color = method, shape = method)) + geom_point(size = 3) +
      geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle("Maximum mis-clustering error (with 1 outlier task)") +
      xlab(TeX(r"($h$)")) + ylab("Error")  + scale_x_continuous(breaks = h) +
      geom_errorbar(aes(ymin=misclustering_max_mean-misclustering_max_sd, ymax=misclustering_max_mean+misclustering_max_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")
  } else if (i == 2) {
    er.combined %>% filter(outlier_num ==i) %>%
      ggplot(aes(x = h, y = misclustering_max_mean, color = method, shape = method)) + geom_point(size = 3) +
      geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle("Maximum mis-clustering error (with 2 outlier tasks)") +
      xlab(TeX(r"($h$)")) + ylab("Error")  + scale_x_continuous(breaks = h) +
      geom_errorbar(aes(ymin=misclustering_max_mean-misclustering_max_sd, ymax=misclustering_max_mean+misclustering_max_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")
  }
}, simplify = F)

plot_misclustering_avg <- sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = misclustering_avg_mean, color = method, shape = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle("Average mis-clustering error") +
    xlab(TeX(r"($h$)")) + ylab("Error")  + scale_x_continuous(breaks = h) +
    geom_errorbar(aes(ymin=misclustering_avg_mean-misclustering_avg_sd, ymax=misclustering_avg_mean+misclustering_avg_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")
}, simplify = F)


plot_legend <- er.combined %>% filter(outlier_num ==0) %>%
  ggplot(aes(x = h, y = w_mean, color = method, shape = method)) + geom_point(size = 3) +
  geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
  lims(x = c(0,0), y = c(0,0))+
  theme_void()+
  theme(legend.position = c(0.5,0.5),
        legend.key.size = unit(0.8, "cm"),
        legend.text = element_text(size =  11),
        legend.title = element_text(size = 13, face = "bold"))

# output a 10 times 5 pdf: mtl_1_main.pdf  Figure 2
ggarrange(plot_beta2[[1]], plot_misclustering_max2[[1]],
          plot_beta2[[3]], plot_misclustering_max2[[3]],
          nrow = 2, ncol = 2, legend = "bottom", common.legend = TRUE)

# output a 10 times 8.5 pdf: mtl_1_0_app.pdf  Figure S.6
ggarrange(plot_w[[1]], plot_mu[[1]],
          plot_Sigma[[1]], plot_delta[[1]],
          plot_misclustering_avg[[1]], plot_legend,
          nrow = 3, ncol = 2)

# output a 10 times 8.5 pdf: mtl_1_2_app.pdf Figure S.7
ggarrange(plot_w[[3]], plot_mu[[3]],
          plot_Sigma[[3]], plot_delta[[3]],
          plot_misclustering_avg[[3]], plot_legend,
          nrow = 3, ncol = 2)

# -------------------------------------------
# Simulation 2: MTL-2, Figures S.8, S.9, and S.10
# -------------------------------------------
er.combined <- load_result(folder_path,
                           experiment_no = "mtl_2")

plot_w <- sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = w_mean, color = method, shape = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{w^{(k)*}_r\}_{r \in \[R\], k \in S}$)")) +
    xlab(TeX(r"($h$)")) + ylab("Error") + scale_x_continuous(breaks = seq(0, 10, 1)) +
    geom_errorbar(aes(ymin=w_mean-w_sd, ymax=w_mean+w_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")
}, simplify = F)



plot_mu <- sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = mu_mean, color = method, shape = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\mu^{(k)*}_r\}_{r \in \[R\], k \in S}$)")) +
    xlab(TeX(r"($h$)")) + ylab("Error") + scale_x_continuous(breaks = seq(0, 10, 1)) +
    geom_errorbar(aes(ymin=mu_mean-mu_sd, ymax=mu_mean+mu_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")
}, simplify = F)


plot_beta2 <-  sapply(0:2, function(i){
  if(i == 0) {
    er.combined %>% filter(outlier_num ==i) %>%
      ggplot(aes(x = h, y = beta_mean, color = method, shape = method)) + geom_point(size = 3) +
      geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\beta^{(k)*}_r\}_{r \in \[R\], k \in S}$ (without outlier tasks))")) +
      xlab(TeX(r"($h$)")) + ylab("Error") + scale_x_continuous(breaks = h) +
      geom_errorbar(aes(ymin=beta_mean-beta_sd, ymax=beta_mean+beta_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")
  } else if (i == 1) {
    er.combined %>% filter(outlier_num ==i) %>%
      ggplot(aes(x = h, y = beta_mean, color = method, shape = method)) + geom_point(size = 3) +
      geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\beta^{(k)*}_r\}_{r \in \[R\], k \in S}$ (with 1 outlier task))")) +
      xlab(TeX(r"($h$)")) + ylab("Error") + scale_x_continuous(breaks = h) +
      geom_errorbar(aes(ymin=beta_mean-beta_sd, ymax=beta_mean+beta_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")
  } else if (i == 2) {
    er.combined %>% filter(outlier_num ==i) %>%
      ggplot(aes(x = h, y = beta_mean, color = method, shape = method)) + geom_point(size = 3) +
      geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\beta^{(k)*}_r\}_{r \in \[R\], k \in S}$ (with 2 outlier tasks))")) +
      xlab(TeX(r"($h$)")) + ylab("Error") + scale_x_continuous(breaks = h) +
      geom_errorbar(aes(ymin=beta_mean-beta_sd, ymax=beta_mean+beta_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")
  }
}, simplify = F)



plot_delta <-  sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = delta_mean, color = method, shape = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\delta^{(k)*}_r\}_{r \in \[R\], k \in S}$)")) +
    xlab(TeX(r"($h$)")) + ylab("Error") + scale_x_continuous(breaks = seq(0, 10, 1)) +
    geom_errorbar(aes(ymin=delta_mean-delta_sd, ymax=delta_mean+delta_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")
}, simplify = F)


plot_Sigma <- sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = Sigma_mean, color = method, shape = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\Sigma^{(k)*}\}_{k \in S}$)")) +
    xlab(TeX(r"($h$)")) + ylab("Error")  + scale_x_continuous(breaks = seq(0, 10, 1)) +
    geom_errorbar(aes(ymin=Sigma_mean-Sigma_sd, ymax=Sigma_mean+Sigma_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")
}, simplify = F)



plot_misclustering_max2 <- sapply(0:2, function(i){
  if (i == 0) {
    er.combined %>% filter(outlier_num ==i) %>%
      ggplot(aes(x = h, y = misclustering_max_mean, color = method, shape = method)) + geom_point(size = 3) +
      geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle("Maximum mis-clustering error (without outlier tasks)") +
      xlab(TeX(r"($h$)")) + ylab("Error")  + scale_x_continuous(breaks = h) +
      geom_errorbar(aes(ymin=misclustering_max_mean-misclustering_max_sd, ymax=misclustering_max_mean+misclustering_max_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")
  } else if (i == 1) {
    er.combined %>% filter(outlier_num ==i) %>%
      ggplot(aes(x = h, y = misclustering_max_mean, color = method, shape = method)) + geom_point(size = 3) +
      geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle("Maximum mis-clustering error (with 1 outlier task)") +
      xlab(TeX(r"($h$)")) + ylab("Error")  + scale_x_continuous(breaks = h) +
      geom_errorbar(aes(ymin=misclustering_max_mean-misclustering_max_sd, ymax=misclustering_max_mean+misclustering_max_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")
  } else if (i == 2) {
    er.combined %>% filter(outlier_num ==i) %>%
      ggplot(aes(x = h, y = misclustering_max_mean, color = method, shape = method)) + geom_point(size = 3) +
      geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle("Maximum mis-clustering error (with 2 outlier tasks)") +
      xlab(TeX(r"($h$)")) + ylab("Error")  + scale_x_continuous(breaks = h) +
      geom_errorbar(aes(ymin=misclustering_max_mean-misclustering_max_sd, ymax=misclustering_max_mean+misclustering_max_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")
  }
}, simplify = F)


plot_misclustering_avg <- sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = misclustering_avg_mean, color = method, shape = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle("Average mis-clustering error") +
    xlab(TeX(r"($h$)")) + ylab("Error") + scale_x_continuous(breaks = seq(0, 10, 1)) +
    geom_errorbar(aes(ymin=misclustering_avg_mean-misclustering_avg_sd, ymax=misclustering_avg_mean+misclustering_avg_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")
}, simplify = F)


plot_legend <- er.combined %>% filter(outlier_num ==0) %>%
  ggplot(aes(x = h, y = w_mean, color = method, shape = method)) + geom_point(size = 3) +
  geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
  lims(x = c(0,0), y = c(0,0))+
  theme_void()+
  theme(legend.position = c(0.5,0.5),
        legend.key.size = unit(0.8, "cm"),
        legend.text = element_text(size =  11),
        legend.title = element_text(size = 13, face = "bold"))



# output a 10 times 5 pdf: mtl_2_main.pdf Figures S.8
ggarrange(plot_beta2[[1]], plot_misclustering_max2[[1]],
          plot_beta2[[3]], plot_misclustering_max2[[3]],
          nrow = 2, ncol = 2, legend = "bottom", common.legend = TRUE)


# output a 10 times 9.5 pdf: mtl_2_0_app Figures S.9
ggarrange(plot_w[[1]], plot_mu[[1]],
          plot_Sigma[[1]], plot_delta[[1]],
          plot_misclustering_avg[[1]], plot_legend,
          nrow = 3, ncol = 2)

# output a 10 times 9.5 pdf: mtl_2_2_app Figures S.10
ggarrange(plot_w[[3]], plot_mu[[3]],
          plot_Sigma[[3]], plot_delta[[3]],
          plot_misclustering_avg[[3]], plot_legend,
          nrow = 3, ncol = 2)


# -------------------------------------------
# Simulation 3: MTL-3, Figures S.11 and S.12
# -------------------------------------------
er.combined <- load_result(folder_path,
                           experiment_no = "mtl_3")

plot_w <- sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = w_mean, color = method, shape = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{w^{(k)*}\}_{k \in S}$)")) +
    xlab(TeX(r"($h$)")) + ylab("Error") + scale_x_continuous(breaks = h) +
    geom_errorbar(aes(ymin=w_mean-w_sd, ymax=w_mean+w_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")
}, simplify = F)



plot_mu <- sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = mu_mean, color = method, shape = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\mu^{(k)*}_1\}_{k \in S}$ and $\{\mu^{(k)*}_2\}_{k \in S}$)")) +
    xlab(TeX(r"($h$)")) + ylab("Error") + scale_x_continuous(breaks = h) +
    geom_errorbar(aes(ymin=mu_mean-mu_sd, ymax=mu_mean+mu_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")
}, simplify = F)





plot_beta <-  sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = beta_mean, color = method, shape = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")+ ggtitle(TeX(r"(Estimation error of $\{\beta^{(k)*}\}_{k \in S}$)")) +
    xlab(TeX(r"($h$)")) + ylab("Error") + scale_x_continuous(breaks = h) +
    geom_errorbar(aes(ymin=beta_mean-beta_sd, ymax=beta_mean+beta_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")
}, simplify = F)


plot_delta <-  sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = delta_mean, color = method, shape = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\delta^{(k)*}\}_{k \in S}$)")) +
    xlab(TeX(r"($h$)")) + ylab("Error") + scale_x_continuous(breaks = h) +
    geom_errorbar(aes(ymin=delta_mean-delta_sd, ymax=delta_mean+delta_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")
}, simplify = F)


plot_Sigma <- sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = Sigma_mean, color = method, shape = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\Sigma^{(k)*}\}_{k \in S}$)")) +
    xlab(TeX(r"($h$)")) + ylab("Error")  + scale_x_continuous(breaks = h) +
    geom_errorbar(aes(ymin=Sigma_mean-Sigma_sd, ymax=Sigma_mean+Sigma_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")
}, simplify = F)



plot_misclustering_max <- sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = misclustering_max_mean, color = method, shape = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle("Maximum mis-clustering error") +
    xlab(TeX(r"($h$)")) + ylab("Error")  + scale_x_continuous(breaks = h) +
    geom_errorbar(aes(ymin=misclustering_max_mean-misclustering_max_sd, ymax=misclustering_max_mean+misclustering_max_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")
}, simplify = F)


plot_misclustering_avg <- sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = misclustering_avg_mean, color = method, shape = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle("Average mis-clustering error") +
    xlab(TeX(r"($h$)")) + ylab("Error")  + scale_x_continuous(breaks = h) +
    geom_errorbar(aes(ymin=misclustering_avg_mean-misclustering_avg_sd, ymax=misclustering_avg_mean+misclustering_avg_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")
}, simplify = F)


plot_legend <- er.combined %>% filter(outlier_num ==0) %>%
  ggplot(aes(x = h, y = w_mean, color = method, shape = method)) + geom_point(size = 3) +
  geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
  lims(x = c(0,0), y = c(0,0))+
  theme_void()+
  theme(legend.position = c(0.5,0.5),
        legend.key.size = unit(0.8, "cm"),
        legend.text = element_text(size =  11),
        legend.title = element_text(size = 13, face = "bold"))

# output a 10 times 11 pdf: mtl_3_0.pdf  Figures S.11
ggarrange(plot_w[[1]], plot_mu[[1]],
          plot_beta[[1]], plot_Sigma[[1]],
          plot_delta[[1]], plot_misclustering_max[[1]],
          plot_misclustering_avg[[1]], plot_legend,
          nrow = 4, ncol = 2)

# output a 10 times 11 pdf: mtl_3_2.pdf  Figures S.12
ggarrange(plot_w[[3]], plot_mu[[3]],
          plot_beta[[3]], plot_Sigma[[3]],
          plot_delta[[3]], plot_misclustering_max[[3]],
          plot_misclustering_avg[[3]], plot_legend,
          nrow = 4, ncol = 2)





# -------------------------------------------
# Simulation: EM with mis-specified cluster numbers, Figure S.5
# -------------------------------------------
nmi_combined <- load_result(folder_path,
                            experiment_no = "mtl_3")


# save as a 8 by 4 pdf: Figure S.5
nmi_combined %>% ggplot(aes(x = h, y = NMI_mean, color = method, shape = method)) + geom_point(size = 3) +
  geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
  xlab(TeX(r"($h$)")) + ylab("Normalized Mutual Information (NMI)") + scale_x_continuous(breaks = seq(0, 10, 1)) +
  geom_errorbar(aes(ymin=NMI_mean-NMI_sd, ymax=NMI_mean+NMI_sd), width = 0.03, size = 0.4) + theme(legend.position = "right") +
  scale_color_manual(values = c(hue_pal()(4), hue_pal()(4))) + scale_shape_manual(values = c(16, 15, 17, 18, 1, 0, 2, 5))



# -------------------------------------------
# Simulation: MTL-diff-R, Figures S.15 and S.16
# -------------------------------------------
er_combined <- load_result(folder_path,
                           experiment_no = "mtl_diff_R")


plot_w <- sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = w_mean, color = method, shape = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{w^{(k)*}_r\}_{r \in \[R\], k \in S}$)")) +
    xlab(TeX(r"($h$)")) + ylab("Error") + scale_x_continuous(breaks = seq(0, 10, 1)) +
    geom_errorbar(aes(ymin=w_mean-w_sd, ymax=w_mean+w_sd), width = 0.03, size = 0.4) + theme(legend.position = "none") +
    scale_color_manual(values = hue_pal()(3)[c(1,3)]) + scale_shape_manual(values = shape_pal()(3)[c(1, 3)])
}, simplify = F)



plot_mu <- sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = mu_mean, color = method, shape = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\mu^{(k)*}_r\}_{r \in \[R\], k \in S}$)")) +
    xlab(TeX(r"($h$)")) + ylab("Error") + scale_x_continuous(breaks = seq(0, 10, 1)) +
    geom_errorbar(aes(ymin=mu_mean-mu_sd, ymax=mu_mean+mu_sd), width = 0.03, size = 0.4) + theme(legend.position = "none") +
    scale_color_manual(values = hue_pal()(3)[c(1,3)]) + scale_shape_manual(values = shape_pal()(3)[c(1, 3)])
}, simplify = F)


plot_beta2 <-  sapply(0:2, function(i){
  if(i == 0) {
    er.combined %>% filter(outlier_num ==i) %>%
      ggplot(aes(x = h, y = beta_mean, color = method, shape = method)) + geom_point(size = 3) +
      geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\beta^{(k)*}_r\}_{r \in \[R\], k \in S}$)")) +
      xlab(TeX(r"($h$)")) + ylab("Error") + scale_x_continuous(breaks = h_list) +
      geom_errorbar(aes(ymin=beta_mean-beta_sd, ymax=beta_mean+beta_sd), width = 0.03, size = 0.4) + theme(legend.position = "none") +
      scale_color_manual(values = hue_pal()(3)[c(1,3)]) + scale_shape_manual(values = shape_pal()(3)[c(1, 3)])
  } else if (i == 1) {
    er.combined %>% filter(outlier_num ==i) %>%
      ggplot(aes(x = h, y = beta_mean, color = method, shape = method)) + geom_point(size = 3) +
      geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\beta^{(k)*}_r\}_{r \in \[R\], k \in S}$)")) +
      xlab(TeX(r"($h$)")) + ylab("Error") + scale_x_continuous(breaks = h_list) +
      geom_errorbar(aes(ymin=beta_mean-beta_sd, ymax=beta_mean+beta_sd), width = 0.03, size = 0.4) + theme(legend.position = "none") +
      scale_color_manual(values = hue_pal()(3)[c(1,3)]) + scale_shape_manual(values = shape_pal()(3)[c(1, 3)])
  } else if (i == 2) {
    er.combined %>% filter(outlier_num ==i) %>%
      ggplot(aes(x = h, y = beta_mean, color = method, shape = method)) + geom_point(size = 3) +
      geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\beta^{(k)*}_r\}_{r \in \[R\], k \in S}$)")) +
      xlab(TeX(r"($h$)")) + ylab("Error") + scale_x_continuous(breaks = h_list) +
      geom_errorbar(aes(ymin=beta_mean-beta_sd, ymax=beta_mean+beta_sd), width = 0.03, size = 0.4) + theme(legend.position = "none") +
      scale_color_manual(values = hue_pal()(3)[c(1,3)]) + scale_shape_manual(values = shape_pal()(3)[c(1, 3)])
  }
}, simplify = F)



plot_delta <-  sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = delta_mean, color = method, shape = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\delta^{(k)*}_r\}_{r \in \[R\], k \in S}$)")) +
    xlab(TeX(r"($h$)")) + ylab("Error") + scale_x_continuous(breaks = seq(0, 10, 1)) +
    geom_errorbar(aes(ymin=delta_mean-delta_sd, ymax=delta_mean+delta_sd), width = 0.03, size = 0.4) + theme(legend.position = "none") +
    scale_color_manual(values = hue_pal()(3)[c(1,3)]) + scale_shape_manual(values = shape_pal()(3)[c(1, 3)])
}, simplify = F)


plot_Sigma <- sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = Sigma_mean, color = method, shape = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\Sigma^{(k)*}\}_{k \in S}$)")) +
    xlab(TeX(r"($h$)")) + ylab("Error")  + scale_x_continuous(breaks = seq(0, 10, 1)) +
    geom_errorbar(aes(ymin=Sigma_mean-Sigma_sd, ymax=Sigma_mean+Sigma_sd), width = 0.03, size = 0.4) + theme(legend.position = "none") +
    scale_color_manual(values = hue_pal()(3)[c(1,3)]) + scale_shape_manual(values = shape_pal()(3)[c(1, 3)])
}, simplify = F)



plot_misclustering_max2 <- sapply(0:2, function(i){
  if (i == 0) {
    er.combined %>% filter(outlier_num ==i) %>%
      ggplot(aes(x = h, y = misclustering_max_mean, color = method, shape = method)) + geom_point(size = 3) +
      geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle("Maximum mis-clustering error") +
      xlab(TeX(r"($h$)")) + ylab("Error")  + scale_x_continuous(breaks = h_list) +
      geom_errorbar(aes(ymin=misclustering_max_mean-misclustering_max_sd, ymax=misclustering_max_mean+misclustering_max_sd), width = 0.03, size = 0.4) + theme(legend.position = "none") +
      scale_color_manual(values = hue_pal()(3)[c(1,3)]) + scale_shape_manual(values = shape_pal()(3)[c(1, 3)])
  } else if (i == 1) {
    er.combined %>% filter(outlier_num ==i) %>%
      ggplot(aes(x = h, y = misclustering_max_mean, color = method, shape = method)) + geom_point(size = 3) +
      geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle("Maximum mis-clustering error") +
      xlab(TeX(r"($h$)")) + ylab("Error")  + scale_x_continuous(breaks = h_list) +
      geom_errorbar(aes(ymin=misclustering_max_mean-misclustering_max_sd, ymax=misclustering_max_mean+misclustering_max_sd), width = 0.03, size = 0.4) + theme(legend.position = "none") +
      scale_color_manual(values = hue_pal()(3)[c(1,3)]) + scale_shape_manual(values = shape_pal()(3)[c(1, 3)])
  } else if (i == 2) {
    er.combined %>% filter(outlier_num ==i) %>%
      ggplot(aes(x = h, y = misclustering_max_mean, color = method, shape = method)) + geom_point(size = 3) +
      geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle("Maximum mis-clustering error") +
      xlab(TeX(r"($h$)")) + ylab("Error")  + scale_x_continuous(breaks = h_list) +
      geom_errorbar(aes(ymin=misclustering_max_mean-misclustering_max_sd, ymax=misclustering_max_mean+misclustering_max_sd), width = 0.03, size = 0.4) + theme(legend.position = "none") +
      scale_color_manual(values = hue_pal()(3)[c(1,3)]) + scale_shape_manual(values = shape_pal()(3)[c(1, 3)])
  }
}, simplify = F)


plot_misclustering_avg <- sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = misclustering_avg_mean, color = method, shape = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle("Average mis-clustering error") +
    xlab(TeX(r"($h$)")) + ylab("Error") + scale_x_continuous(breaks = seq(0, 10, 1)) +
    geom_errorbar(aes(ymin=misclustering_avg_mean-misclustering_avg_sd, ymax=misclustering_avg_mean+misclustering_avg_sd), width = 0.03, size = 0.4) + theme(legend.position = "none") +
    scale_color_manual(values = hue_pal()(3)[c(1,3)]) + scale_shape_manual(values = shape_pal()(3)[c(1, 3)])
}, simplify = F)


plot_legend <- er.combined %>% filter(outlier_num ==0) %>%
  ggplot(aes(x = h, y = w_mean, color = method, shape = method)) + geom_point(size = 3) +
  geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
  lims(x = c(0,0), y = c(0,0))+
  theme_void()+
  theme(legend.position = c(0.5,0.5),
        legend.key.size = unit(0.8, "cm"),
        legend.text = element_text(size =  11),
        legend.title = element_text(size = 13, face = "bold")) +
  scale_color_manual(values = hue_pal()(3)[c(1,3)]) + scale_shape_manual(values = shape_pal()(3)[c(1, 3)])


# output a 10 times 10 pdf: mtl_diff_R_0  Figure S.15
ggarrange(plot_w[[1]], plot_mu[[1]],
          plot_Sigma[[1]], plot_delta[[1]],
          plot_beta2[[1]], plot_misclustering_max2[[1]],
          plot_misclustering_avg[[1]], plot_legend,
          nrow = 4, ncol = 2)

# output a 10 times 10 pdf: mtl_diff_R_2  Figure S.16
ggarrange(plot_w[[3]], plot_mu[[3]],
          plot_Sigma[[3]], plot_delta[[3]],
          plot_beta2[[3]], plot_misclustering_max2[[3]],
          plot_misclustering_avg[[3]], plot_legend,
          nrow = 4, ncol = 2)

# -------------------------------------------
# Simulation: MTL-repre, Table S.2
# -------------------------------------------
L <- load_result(folder_path,
                 experiment_no = "mtl_repre")
er.single <- L[[1]]
er.single.sd <- L[[2]]
er.mtlgmm <- L[[3]]
er.mtlgmm.sd <- L[[4]]

for (i in 1:nrow(C_matrix)) {
  cat("(")
  cat(C_matrix[i, ], sep = ", ")
  cat(") ")
  for (j in 1:2) {
    cat("& ", round(er.single[i, j], digits = 3), " (", round(er.single.sd[i, j], digits = 3), ") ", sep = "")
    cat("& ", round(er.mtlgmm[i, j], digits = 3), " (", round(er.mtlgmm.sd[i, j], digits = 3), ") ", sep = "")
  }
  cat("\\\\ \n")
}


# -------------------------------------------
# Simulation: MTL-1-epsilon, Figure S.13
# -------------------------------------------
er.combined <- load_result(folder_path,
                           experiment_no = "mtl_1_epsilon")


plot_beta <- er.combined %>% ggplot(aes(x = epsilon, y = beta_mean, color = method, shape = method, group = method)) + geom_point(size = 3) +
  geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\beta^{(k)*}\}_{k \in S}$)")) +
  xlab(TeX(r"($\epsilon$)")) + ylab("Error")  +
  geom_errorbar(aes(ymin=beta_mean-beta_sd, ymax=beta_mean+beta_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")

plot_w <- er.combined %>% ggplot(aes(x = epsilon, y = w_mean, color = method, shape = method, group = method)) + geom_point(size = 3) +
  geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
  xlab(TeX(r"($\epsilon$)")) + ylab("Error") + ggtitle(TeX(r"(Estimation error of $\{w^{(k)*}\}_{k \in S}$)")) +
  geom_errorbar(aes(ymin=w_mean-w_sd, ymax=w_mean+w_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")


plot_mu <- er.combined %>% ggplot(aes(x = epsilon, y = mu_mean, color = method, shape = method, group = method)) + geom_point(size = 3) +
  geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\mu^{(k)*}_1\}_{k \in S}$ and $\{\mu^{(k)*}_2\}_{k \in S}$)")) +
  xlab(TeX(r"($\epsilon$)")) + ylab("Error")  +
  geom_errorbar(aes(ymin=mu_mean-mu_sd, ymax=mu_mean+mu_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")


plot_delta <- er.combined %>% ggplot(aes(x = epsilon, y = delta_mean, color = method, shape = method, group = method)) + geom_point(size = 3) +
  geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\delta^{(k)*}\}_{k \in S}$)")) +
  xlab(TeX(r"($\epsilon$)")) + ylab("Error")  +
  geom_errorbar(aes(ymin=delta_mean-delta_sd, ymax=delta_mean+delta_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")




plot_Sigma <- er.combined %>% ggplot(aes(x = epsilon, y = Sigma_mean, color = method, shape = method, group = method)) + geom_point(size = 3) +
  geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\Sigma^{(k)*}\}_{k \in S}$)")) +
  xlab(TeX(r"($\epsilon$)")) + ylab("Error")  +
  geom_errorbar(aes(ymin=Sigma_mean-Sigma_sd, ymax=Sigma_mean+Sigma_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")


plot_misclustering_max <- er.combined %>% ggplot(aes(x = epsilon, y = misclustering_max_mean, color = method, shape = method, group = method)) + geom_point(size = 3) +
  geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle("Maximum mis-clustering error") +
  xlab(TeX(r"($\epsilon$)")) + ylab("Error") + theme(legend.position = "none") +
  geom_errorbar(aes(ymin=misclustering_max_mean-misclustering_max_sd, ymax=misclustering_max_mean+misclustering_max_sd), width = 0.03, size = 0.4)


plot_misclustering_avg <- er.combined %>% ggplot(aes(x = epsilon, y = misclustering_avg_mean, color = method, shape = method, group = method)) + geom_point(size = 3) +
  geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle("Average mis-clustering error") +
  xlab(TeX(r"($\epsilon$)")) + ylab("Error") + theme(legend.position = "none") +
  geom_errorbar(aes(ymin=misclustering_avg_mean-misclustering_avg_sd, ymax=misclustering_avg_mean+misclustering_avg_sd), width = 0.03, size = 0.4)




plot_legend <- er.combined %>%
  ggplot(aes(x = epsilon, y = w_mean, color = method, shape = method)) + geom_point(size = 3) +
  geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
  lims(x = c(0,0), y = c(0,0))+
  theme_void()+
  theme(legend.position = c(0.5,0.5),
        legend.key.size = unit(0.8, "cm"),
        legend.text = element_text(size =  11),
        legend.title = element_text(size = 13, face = "bold"))


# output a 10 times 10 pdf: mtl_1_epsilon.pdf  Figure S.13
ggarrange(plot_w, plot_mu,
          plot_Sigma, plot_delta,
          plot_beta, plot_misclustering_max,
          plot_misclustering_avg, plot_legend,
          nrow = 4, ncol = 2)


# -------------------------------------------
# Simulation: MTL-epsilon  Figure S.14
# -------------------------------------------
er.combined <- load_result(folder_path,
                           experiment_no = "mtl_epsilon")


plot_beta <- er.combined %>% ggplot(aes(x = epsilon, y = beta_mean, color = method, shape = method, group = method)) + geom_point(size = 3) +
  geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\beta^{(k)*}\}_{k \in S}$)")) +
  xlab(TeX(r"($\epsilon$)")) + ylab("Error")  +
  geom_errorbar(aes(ymin=beta_mean-beta_sd, ymax=beta_mean+beta_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")

plot_w <- er.combined %>% ggplot(aes(x = epsilon, y = w_mean, color = method, shape = method, group = method)) + geom_point(size = 3) +
  geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
  xlab(TeX(r"($\epsilon$)")) + ylab("Error") + ggtitle(TeX(r"(Estimation error of $\{w^{(k)*}\}_{k \in S}$)")) +
  geom_errorbar(aes(ymin=w_mean-w_sd, ymax=w_mean+w_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")


plot_mu <- er.combined %>% ggplot(aes(x = epsilon, y = mu_mean, color = method, shape = method, group = method)) + geom_point(size = 3) +
  geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\mu^{(k)*}_1\}_{k \in S}$ and $\{\mu^{(k)*}_2\}_{k \in S}$)")) +
  xlab(TeX(r"($\epsilon$)")) + ylab("Error")  +
  geom_errorbar(aes(ymin=mu_mean-mu_sd, ymax=mu_mean+mu_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")


plot_delta <- er.combined %>% ggplot(aes(x = epsilon, y = delta_mean, color = method, shape = method, group = method)) + geom_point(size = 3) +
  geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\delta^{(k)*}\}_{k \in S}$)")) +
  xlab(TeX(r"($\epsilon$)")) + ylab("Error")  +
  geom_errorbar(aes(ymin=delta_mean-delta_sd, ymax=delta_mean+delta_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")




plot_Sigma <- er.combined %>% ggplot(aes(x = epsilon, y = Sigma_mean, color = method, shape = method, group = method)) + geom_point(size = 3) +
  geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\Sigma^{(k)*}\}_{k \in S}$)")) +
  xlab(TeX(r"($\epsilon$)")) + ylab("Error")  +
  geom_errorbar(aes(ymin=Sigma_mean-Sigma_sd, ymax=Sigma_mean+Sigma_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")


plot_misclustering_max <- er.combined %>% ggplot(aes(x = epsilon, y = misclustering_max_mean, color = method, shape = method, group = method)) + geom_point(size = 3) +
  geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle("Maximum mis-clustering error") +
  xlab(TeX(r"($\epsilon$)")) + ylab("Error") + theme(legend.position = "none") +
  geom_errorbar(aes(ymin=misclustering_max_mean-misclustering_max_sd, ymax=misclustering_max_mean+misclustering_max_sd), width = 0.03, size = 0.4)


plot_misclustering_avg <- er.combined %>% ggplot(aes(x = epsilon, y = misclustering_avg_mean, color = method, shape = method, group = method)) + geom_point(size = 3) +
  geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle("Average mis-clustering error") +
  xlab(TeX(r"($\epsilon$)")) + ylab("Error") + theme(legend.position = "none") +
  geom_errorbar(aes(ymin=misclustering_avg_mean-misclustering_avg_sd, ymax=misclustering_avg_mean+misclustering_avg_sd), width = 0.03, size = 0.4)




plot_legend <- er.combined %>%
  ggplot(aes(x = epsilon, y = w_mean, color = method, shape = method)) + geom_point(size = 3) +
  geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
  lims(x = c(0,0), y = c(0,0))+
  theme_void()+
  theme(legend.position = c(0.5,0.5),
        legend.key.size = unit(0.8, "cm"),
        legend.text = element_text(size =  11),
        legend.title = element_text(size = 13, face = "bold"))



# output a 8 times 9 pdf: mtl_epsilon.pdf Figure S.14
ggarrange(plot_w[[1]], plot_mu[[1]],
          plot_beta[[1]], plot_Sigma[[1]],
          plot_delta[[1]], plot_misclustering[[1]],
          common.legend = TRUE, legend = "bottom", nrow = 3, ncol = 2)

# -------------------------------------------
# Simulation: TL-1, Figures S.17 and S.18
# -------------------------------------------
er.combined <- load_result(folder_path,
                           experiment_no = "tl_1")

plot_w <- sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = w_mean, color = method, shape = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $w^{(0)*}$)")) +
    xlab(TeX(r"($h$)")) + ylab("Error") + scale_x_continuous(breaks = seq(0, 10, 1)) +
    geom_errorbar(aes(ymin=w_mean-w_sd, ymax=w_mean+w_sd), width = 0.03, size = 0.4) + theme(legend.position = "none") +
    scale_color_manual(values=c("#F8766D", "#FCCF05", "#00BA38", "#619CFF", "#FC99FF"))
}, simplify = F)


plot_mu <- sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = mu_mean, color = method, shape = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\mu^{(0)*}_1$ and $\mu^{(0)*}_2$)")) +
    xlab(TeX(r"($h$)")) + ylab("Error") + scale_x_continuous(breaks = seq(0, 10, 1)) +
    geom_errorbar(aes(ymin=mu_mean-mu_sd, ymax=mu_mean+mu_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")+
    scale_color_manual(values=c("#F8766D", "#FCCF05", "#00BA38", "#619CFF", "#FC99FF"))
}, simplify = F)



plot_beta <- sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = beta_mean, color = method, shape = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\beta^{(0)*}$)")) +
    xlab(TeX(r"($h$)")) + ylab("Error")  + scale_x_continuous(breaks = seq(0, 10, 1)) +
    geom_errorbar(aes(ymin=beta_mean-beta_sd, ymax=beta_mean+beta_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")+
    scale_color_manual(values=c("#F8766D", "#FCCF05", "#00BA38", "#619CFF", "#FC99FF"))
}, simplify = F)



plot_Sigma <- sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = Sigma_mean, color = method, shape = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\Sigma^{(0)*}$)")) +
    xlab(TeX(r"($h$)")) + ylab("Error")  + scale_x_continuous(breaks = seq(0, 10, 1)) +
    geom_errorbar(aes(ymin=Sigma_mean-Sigma_sd, ymax=Sigma_mean+Sigma_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")+
    scale_color_manual(values=c("#F8766D", "#FCCF05", "#00BA38", "#619CFF", "#FC99FF"))
}, simplify = F)


plot_delta <- sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = delta_mean, color = method, shape = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\delta^{(0)*}$)")) +
    xlab(TeX(r"($h$)")) + ylab("Error")  + scale_x_continuous(breaks = seq(0, 10, 1)) +
    geom_errorbar(aes(ymin=delta_mean-delta_sd, ymax=delta_mean+delta_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")+
    scale_color_manual(values=c("#F8766D", "#FCCF05", "#00BA38", "#619CFF", "#FC99FF"))
}, simplify = F)

plot_misclustering <- sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = misclustering_mean, color = method, shape = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle("Mis-clustering error") +
    xlab(TeX(r"($h$)")) + ylab("Error")  + scale_x_continuous(breaks = seq(0, 10, 1)) +
    geom_errorbar(aes(ymin=misclustering_mean-misclustering_sd, ymax=misclustering_mean+misclustering_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")+
    scale_color_manual(values=c("#F8766D", "#FCCF05", "#00BA38", "#619CFF", "#FC99FF"))
}, simplify = F)


plot_legend <- er.combined %>% filter(outlier_num ==0, method != "MTL-GMM-center") %>%
  ggplot(aes(x = h, y = w_mean, color = method, shape = method)) + geom_point(size = 3) +
  geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
  lims(x = c(0,0), y = c(0,0))+ scale_color_manual(values=c("#F8766D", "#00BA38", "#619CFF", "#FC99FF")) +
  theme_void()+
  theme(legend.position = c(0.5,0.5),
        legend.key.size = unit(0.8, "cm"),
        legend.text = element_text(size =  11),
        legend.title = element_text(size = 13, face = "bold"))


# output a 8 times 9 pdf: tl_1_0.pdf  Figure S.17
ggarrange(plot_w[[1]], plot_mu[[1]],
          plot_beta[[1]], plot_Sigma[[1]],
          plot_delta[[1]], plot_misclustering[[1]],
          common.legend = TRUE, legend = "bottom", nrow = 3, ncol = 2)

# output a 8 times 9 pdf: tl_1_2  Figure S.18
ggarrange(plot_w[[3]], plot_mu[[3]],
          plot_beta[[3]], plot_Sigma[[3]],
          plot_delta[[3]], plot_misclustering[[3]],
          common.legend = TRUE, legend = "bottom", nrow = 3, ncol = 2)

# -------------------------------------------
# Simulation 1: MTL-1 testing tuning parameter -- C_lambda, Figure S.19
# -------------------------------------------
er.combined <- load_result(folder_path,
                           experiment_no = "mtl_1_tuning_C")

plot_w <- sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = w_mean, color = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{w^{(k)*}\}_{k \in S}$)")) +
    xlab(TeX(r"($h$)")) + ylab("Error") + scale_x_continuous(breaks = h) +
    geom_errorbar(aes(ymin=w_mean-w_sd, ymax=w_mean+w_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")+
    scale_color_manual(values=c("#F8766D", "#C49A00", "#FCCF05", "#A58AFF", "#FEA3E5","#00BA38", "#619CFF"))
}, simplify = F)



plot_mu <- sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = mu_mean, color = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\mu^{(k)*}_1\}_{k \in S}$ and $\{\mu^{(k)*}_2\}_{k \in S}$)")) +
    xlab(TeX(r"($h$)")) + ylab("Error") + scale_x_continuous(breaks = h) +
    geom_errorbar(aes(ymin=mu_mean-mu_sd, ymax=mu_mean+mu_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")+
    scale_color_manual(values=c("#F8766D", "#C49A00", "#FCCF05", "#A58AFF", "#FEA3E5","#00BA38", "#619CFF"))
}, simplify = F)




plot_beta <-  sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = beta_mean, color = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\beta^{(k)*}\}_{k \in S}$)"))  +
    xlab(TeX(r"($h$)")) + ylab("Error") + scale_x_continuous(breaks = h) +
    geom_errorbar(aes(ymin=beta_mean-beta_sd, ymax=beta_mean+beta_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")+
    scale_color_manual(values=c("#F8766D", "#C49A00", "#FCCF05", "#A58AFF", "#FEA3E5","#00BA38", "#619CFF"))
}, simplify = F)


plot_delta <-  sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = delta_mean, color = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\delta^{(k)*}\}_{k \in S}$)")) +
    xlab(TeX(r"($h$)")) + ylab("Error") + scale_x_continuous(breaks = h) +
    geom_errorbar(aes(ymin=delta_mean-delta_sd, ymax=delta_mean+delta_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")+
    scale_color_manual(values=c("#F8766D", "#C49A00", "#FCCF05", "#A58AFF", "#FEA3E5","#00BA38", "#619CFF"))
}, simplify = F)


plot_Sigma <- sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = Sigma_mean, color = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\Sigma^{(k)*}\}_{k \in S}$)")) +
    xlab(TeX(r"($h$)")) + ylab("Error")  + scale_x_continuous(breaks = h) +
    geom_errorbar(aes(ymin=Sigma_mean-Sigma_sd, ymax=Sigma_mean+Sigma_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")+
    scale_color_manual(values=c("#F8766D", "#C49A00", "#FCCF05", "#A58AFF", "#FEA3E5","#00BA38", "#619CFF"))
}, simplify = F)



plot_misclustering_max <- sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = misclustering_max_mean, color = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle("Maximum mis-clustering error")  +
    xlab(TeX(r"($h$)")) + ylab("Error")  + scale_x_continuous(breaks = h) +
    geom_errorbar(aes(ymin=misclustering_max_mean-misclustering_max_sd, ymax=misclustering_max_mean+misclustering_max_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")+
    scale_color_manual(values=c("#F8766D", "#C49A00", "#FCCF05", "#A58AFF", "#FEA3E5","#00BA38", "#619CFF"))
}, simplify = F)

plot_misclustering_avg <- sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = misclustering_avg_mean, color = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle("Average mis-clustering error") +
    xlab(TeX(r"($h$)")) + ylab("Error")  + scale_x_continuous(breaks = h) +
    geom_errorbar(aes(ymin=misclustering_avg_mean-misclustering_avg_sd, ymax=misclustering_avg_mean+misclustering_avg_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")+
    scale_color_manual(values=c("#F8766D", "#C49A00", "#FCCF05", "#A58AFF", "#FEA3E5","#00BA38", "#619CFF"))
}, simplify = F)


plot_legend <- er.combined %>% filter(outlier_num ==0) %>%
  ggplot(aes(x = h, y = w_mean, color = method)) + geom_point(size = 3) +
  geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
  lims(x = c(0,0), y = c(0,0))+
  theme_void()+
  theme(legend.position = c(0.5,0.5),
        legend.key.size = unit(0.8, "cm"),
        legend.text = element_text(size =  11),
        legend.title = element_text(size = 13, face = "bold")) +
  scale_color_manual(values=c("#F8766D", "#C49A00", "#FCCF05", "#A58AFF", "#FEA3E5","#00BA38", "#619CFF"))

# output a 10 times 11 pdf: mtl_1_tuning_C.pdf, Figure S.19
ggarrange(plot_w[[1]], plot_mu[[1]],
          plot_beta[[1]], plot_Sigma[[1]],
          plot_delta[[1]], plot_misclustering_max[[1]],
          plot_misclustering_avg[[1]], plot_legend,
          nrow = 4, ncol = 2)


# -------------------------------------------
# Simulation 1: MTL-1 testing tuning parameter -- kappa, Figure S.20
# -------------------------------------------
er.combined <- load_result(folder_path,
                           experiment_no = "mtl_1_tuning_kappa")

plot_w <- sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = w_mean, color = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{w^{(k)*}\}_{k \in S}$)")) +
    xlab(TeX(r"($h$)")) + ylab("Error") + scale_x_continuous(breaks = h) +
    geom_errorbar(aes(ymin=w_mean-w_sd, ymax=w_mean+w_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")+
    scale_color_manual(values=c("#F8766D", "#C49A00", "#FCCF05", "#A58AFF", "#FEA3E5","#00BA38", "#619CFF"))
}, simplify = F)



plot_mu <- sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = mu_mean, color = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\mu^{(k)*}_1\}_{k \in S}$ and $\{\mu^{(k)*}_2\}_{k \in S}$)")) +
    xlab(TeX(r"($h$)")) + ylab("Error") + scale_x_continuous(breaks = h) +
    geom_errorbar(aes(ymin=mu_mean-mu_sd, ymax=mu_mean+mu_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")+
    scale_color_manual(values=c("#F8766D", "#C49A00", "#FCCF05", "#A58AFF", "#FEA3E5","#00BA38", "#619CFF"))
}, simplify = F)




plot_beta <-  sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = beta_mean, color = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\beta^{(k)*}\}_{k \in S}$)"))  +
    xlab(TeX(r"($h$)")) + ylab("Error") + scale_x_continuous(breaks = h) +
    geom_errorbar(aes(ymin=beta_mean-beta_sd, ymax=beta_mean+beta_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")+
    scale_color_manual(values=c("#F8766D", "#C49A00", "#FCCF05", "#A58AFF", "#FEA3E5","#00BA38", "#619CFF"))
}, simplify = F)


plot_delta <-  sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = delta_mean, color = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\delta^{(k)*}\}_{k \in S}$)")) +
    xlab(TeX(r"($h$)")) + ylab("Error") + scale_x_continuous(breaks = h) +
    geom_errorbar(aes(ymin=delta_mean-delta_sd, ymax=delta_mean+delta_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")+
    scale_color_manual(values=c("#F8766D", "#C49A00", "#FCCF05", "#A58AFF", "#FEA3E5","#00BA38", "#619CFF"))
}, simplify = F)


plot_Sigma <- sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = Sigma_mean, color = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle(TeX(r"(Estimation error of $\{\Sigma^{(k)*}\}_{k \in S}$)")) +
    xlab(TeX(r"($h$)")) + ylab("Error")  + scale_x_continuous(breaks = h) +
    geom_errorbar(aes(ymin=Sigma_mean-Sigma_sd, ymax=Sigma_mean+Sigma_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")+
    scale_color_manual(values=c("#F8766D", "#C49A00", "#FCCF05", "#A58AFF", "#FEA3E5","#00BA38", "#619CFF"))
}, simplify = F)



plot_misclustering_max <- sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = misclustering_max_mean, color = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle("Maximum mis-clustering error")  +
    xlab(TeX(r"($h$)")) + ylab("Error")  + scale_x_continuous(breaks = h) +
    geom_errorbar(aes(ymin=misclustering_max_mean-misclustering_max_sd, ymax=misclustering_max_mean+misclustering_max_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")+
    scale_color_manual(values=c("#F8766D", "#C49A00", "#FCCF05", "#A58AFF", "#FEA3E5","#00BA38", "#619CFF"))
}, simplify = F)

plot_misclustering_avg <- sapply(0:2, function(i){
  er.combined %>% filter(outlier_num ==i) %>%
    ggplot(aes(x = h, y = misclustering_avg_mean, color = method)) + geom_point(size = 3) +
    geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") + ggtitle("Average mis-clustering error") +
    xlab(TeX(r"($h$)")) + ylab("Error")  + scale_x_continuous(breaks = h) +
    geom_errorbar(aes(ymin=misclustering_avg_mean-misclustering_avg_sd, ymax=misclustering_avg_mean+misclustering_avg_sd), width = 0.03, size = 0.4) + theme(legend.position = "none")+
    scale_color_manual(values=c("#F8766D", "#C49A00", "#FCCF05", "#A58AFF", "#FEA3E5","#00BA38", "#619CFF"))
}, simplify = F)


plot_legend <- er.combined %>% filter(outlier_num ==0) %>%
  ggplot(aes(x = h, y = w_mean, color = method)) + geom_point(size = 3) +
  geom_line(size = 1) + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
  lims(x = c(0,0), y = c(0,0))+
  theme_void()+
  theme(legend.position = c(0.5,0.5),
        legend.key.size = unit(0.8, "cm"),
        legend.text = element_text(size =  11),
        legend.title = element_text(size = 13, face = "bold")) +
  scale_color_manual(values=c("#F8766D", "#C49A00", "#FCCF05", "#A58AFF", "#FEA3E5","#00BA38", "#619CFF"))


# output a 10 times 11 pdf: mtl_1_tuning_kappa.pdf, Figure S.20
ggarrange(plot_w[[1]], plot_mu[[1]],
          plot_beta[[1]], plot_Sigma[[1]],
          plot_delta[[1]], plot_misclustering_max[[1]],
          plot_misclustering_avg[[1]], plot_legend,
          nrow = 4, ncol = 2)

# -------------------------------------------
# real data: Human activity recognition, binary problem
# -------------------------------------------
L <- load_result(folder_path,
                 experiment_no = "har")
er.har <- L[[1]]
er.har.sd <- L[[2]]
er.har.all <- L[[3]]

# table: part of Table S.3
er.har

er.har.sd

# plot
# box plot: save as a 8*3 pdf file: har_box_plot_binary.pdf  Figure 3
er.har.all %>% ggplot(aes(x=method,y=error)) + geom_boxplot(aes(color=method)) + theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(vjust=-0.8)) +
  xlab("Method") + ylab("Mis-clustering error") + scale_x_discrete(labels=c("MTL-GMM","Pooled-GMM","Single-task-GMM")) +
  ggtitle("Binary")

# box plot: save as a 8*3 pdf file: Left subfigure of Figure S.21
har_box_plot <- er.har.all %>% ggplot(aes(x=method,y=error)) + geom_boxplot(aes(color=method)) + theme(plot.title = element_text(hjust = 0.5), legend.position="", axis.title.x = element_text(vjust=-0.8)) +
  xlab("Method") + ylab("Mis-clustering error") + scale_x_discrete(labels=c("MTL-GMM","Pooled-GMM","Single-task-GMM")) +
  ggtitle("Binary")


# -------------------------------------------
# real data: Human activity recognition, multi-cluster problem
# -------------------------------------------
L <- load_result(folder_path,
                 experiment_no = "har_multi")
er.har <- L[[1]]
er.har.sd <- L[[2]]
er.har.all <- L[[3]]

# table: part of Table S.3
er.har
er.har.sd


# plot
# box plot: save as a 8*3 pdf file: Right subfigure of Figure S.21
har_multi_box_plot <- er.har.all %>% ggplot(aes(x=method,y=error)) + geom_boxplot(aes(color=method)) + theme(plot.title = element_text(hjust = 0.5), legend.position="", axis.title.x = element_text(vjust=-0.8)) +
  xlab("Method") + ylab("Mis-clustering error") + scale_x_discrete(labels=c("MTL-GMM","Pooled-GMM","Single-task-GMM")) +
  ggtitle("Multi-cluster")


# combine two box plots: save as a 8*3 pdf file ->  har_box_plot.pdf  Figure S.21
ggarrange(har_box_plot, har_multi_box_plot, common.legend = TRUE, legend = "bottom", nrow = 1, ncol = 2)


# -------------------------------------------
# real data: Pen-based recognition of handwritten digits, binary problem
# -------------------------------------------
L <- load_result(folder_path,
                 experiment_no = "pen")
er.pen <- L[[1]]
er.pen.sd <- L[[2]]
er.pen.all <- L[[3]]


# table: part of Table S.4
er.pen
er.pen.sd

# plot
# box plot: save as a 8*3 pdf file: Left subfigure of Figure S.22
pen_box_plot <- er.pen.all %>% ggplot(aes(x=method,y=error)) + geom_boxplot(aes(color=method)) + theme(plot.title = element_text(hjust = 0.5), legend.position="", axis.title.x = element_text(vjust=-0.8)) +
  xlab("Method") + ylab("Mis-clustering error") + scale_x_discrete(labels=c("MTL-GMM","Pooled-GMM","Single-task-GMM")) +
  ggtitle("Binary")


# -------------------------------------------
# real data: Pen-based recognition of handwritten digits, multi-cluster problem
# -------------------------------------------
L <- load_result(folder_path,
                 experiment_no = "pen_multi")
er.pen <- L[[1]]
er.pen.sd <- L[[2]]
er.pen.all <- L[[3]]


# table: part of Table S.4
er.pen

er.pen.sd


# plot
# box plot: save as a 8*3 pdf file: Right subfigure of Figure S.22
pen_multi_box_plot <- er.pen.all %>% ggplot(aes(x=method,y=error)) + geom_boxplot(aes(color=method)) + theme(plot.title = element_text(hjust = 0.5), legend.position="", axis.title.x = element_text(vjust=-0.8)) +
  xlab("Method") + ylab("Mis-clustering error") + scale_x_discrete(labels=c("MTL-GMM","Pooled-GMM","Single-task-GMM")) +
  ggtitle("Multi-cluster")

# combine two box plots: save as a 8*3 pdf file: pen_box_plot.pdf  Figure S.22
ggarrange(pen_box_plot, pen_multi_box_plot, common.legend = TRUE, legend = "bottom", nrow = 1, ncol = 2)




