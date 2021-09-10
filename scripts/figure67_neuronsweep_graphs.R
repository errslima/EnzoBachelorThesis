##############################.
#
#   Main model test
#
#   Enzo Lima
#   Daniel Redhead
#   Subhajit Paul
#
##############################.

# Clear workspace 
rm(list = ls())

load("/home/enzo_lima/data/2021_08_07_neuronsweep_acc_36_8_1d.rda")
error_acc_list1 <- error_acc_list
load("/home/enzo_lima/data/2021_08_07_neuronsweep_acc_36_8_2d.rda")
error_acc_list2 <- error_acc_list
load("/home/enzo_lima/data/2021_08_06_neuronsweep_acc_36_8_3d.rda")
error_acc_list3 <- error_acc_list

accuracy_mean1 <- apply(error_acc_list1[1:29,,], c(1,2), mean)
accuracy_sd1 <- apply(error_acc_list1[1:29,,], c(1,2), sd)

accuracy_mean2 <- apply(error_acc_list2[1:29,,], c(1,2), mean)
accuracy_sd2 <- apply(error_acc_list2[1:29,,], c(1,2), sd)

accuracy_mean3 <- apply(error_acc_list3[1:29,,], c(1,2), mean)
accuracy_sd3 <- apply(error_acc_list3[1:29,,], c(1,2), sd)

data1 <- data.frame(x = 36:8, mean = accuracy_mean1[,100] / 100, sd = accuracy_sd1[,100] / 100, h = 0, alpha = 1, stringsAsFactors = F)
data2 <- data.frame(x = 36:8, mean = accuracy_mean2[,100] / 100, sd = accuracy_sd2[,100] / 100, h = 1, alpha = 1, stringsAsFactors = F)
data3 <- data.frame(x = 36:8, mean = accuracy_mean3[,100] / 100, sd = accuracy_sd3[,100] / 100, h = 2, alpha = 1, stringsAsFactors = F)

data <- do.call(rbind, list(data1, data2, data3))

p <- ggplot(data, aes(x = as.factor(x), y = mean, fill = factor(h))) +
  geom_bar(position = "dodge", stat = "identity", alpha = data$alpha) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), color = "#000000", position = "dodge", stat = "identity", alpha = 0.5) +
  #geom_smooth(data = data2[20:29,], aes(x = x - 7, y = mean), color = "#000000", method = "lm", se = F, formula = y~x, fullrange = T, size = 0.2) +
  #geom_smooth(data = data2[1:10,], aes(x = x - 7, y = mean), color = "#000000", method = "lm", se = F, formula = y~x, fullrange = T, size = 0.2) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  labs(x = "neurons",
       y = "error",
       fill = "hidden layers",
       color = "hidden layers")

png(filename = "./plots/neuronsweep_8_36.png", width = 2048, height = 512, res = 150, type = "cairo")
p
dev.off()

p <- ggplot(data, aes(x = as.factor(x), y = mean, fill = factor(h))) +
  geom_bar(position = "dodge", stat = "identity", alpha = data$alpha) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), color = "#000000", position = "dodge", stat = "identity", alpha = 0.5) +
  geom_smooth(data = data2[20:29,], aes(x = x - 7, y = mean), color = "#000000", method = "lm", se = F, formula = y~x, fullrange = T, size = 0.2) +
  geom_smooth(data = data2[1:10,], aes(x = x - 7, y = mean), color = "#000000", method = "lm", se = F, formula = y~x, fullrange = T, size = 0.2) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  labs(x = "neurons",
       y = "error",
       fill = "hidden layers",
       color = "hidden layers")


png(filename = "./plots/neuronsweep_optimum_8_36.png", width = 2048, height = 2048, res = 150, type = "cairo")
p
dev.off()


# END OF SCRIPT