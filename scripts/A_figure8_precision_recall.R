##############################.
#
#   Precision-Recall graphs (simulated data)
#
#   Enzo Lima
#   Daniel Redhead
#   Subhajit Paul
#
##############################.

library(tidyverse)
library(stringdist)
library(torch)
library(ggplot2)

# Clear workspace 
rm(list = ls())

source("./R/functions.R")
source("./R/predrec.R")

set.seed(69)
load("./test_data/testdata_simulated.rda")
colnames(matched_names) <- c("nomination", "match")
matched_names <- matched_names[1:500,]
unique_ext <- sample(unique(matched_names$match), 50)
matched_names$match[matched_names$match %in% unique_ext] <- NA

nominations <- unique(matched_names$nomination)
alters <- unique(matched_names$match)
alters <- alters[!is.na(alters)]

data <- expand.grid(nominations, alters, stringsAsFactors = F)
pred <- matrix(0, nrow = 500, ncol = 209)
truth <- matrix(0, nrow = 500, ncol = 209)

for(a in 1:500){
  for(b in 1:209){
    if(alters[b] %in% c(0, matched_names$match[matched_names$nomination == nominations[a]])){
      truth[a,b] <- 1
    }
  }
}

pred_lv <- matrix((1 - as.numeric(stringdistnorm(data$Var1, data$Var2, method = "lv"))), nrow = 500, ncol = 209)
pred_jw <- matrix((1 - as.numeric(stringdist(data$Var1, data$Var2, method = "jw"))), nrow = 500, ncol = 209)
pred_c2 <- matrix((1 - as.numeric(stringdist(data$Var1, data$Var2, method = "cosine", q = 2))), nrow = 500, ncol = 209)
pred_c3 <- matrix((1 - as.numeric(stringdist(data$Var1, data$Var2, method = "cosine", q = 3))), nrow = 500, ncol = 209)
pred_n2 <- matrix(1 - as.numeric(stringdist(data$Var1, data$Var2, method = "jaccard", q = 2)), nrow = 500, ncol = 209)
pred_n3 <- matrix(1 - as.numeric(stringdistnorm(data$Var1, data$Var2, method = "jaccard", q = 3)), nrow = 500, ncol = 209)
pred_lcs <- matrix((1 - as.numeric(stringdistnorm(data$Var1, data$Var2, method = "lcs"))), nrow = 500, ncol = 209)

pred_lvplus <- matrix(string_compare(data$Var1, data$Var2, method = "lv") / 100, nrow = 500, ncol = 209)
pred_jwplus <- matrix(string_compare(data$Var1, data$Var2, method = "jw") / 100, nrow = 500, ncol = 209)
pred_c2plus <- matrix((string_compare(data$Var1, data$Var2, method = "cos", q = 2) / 100), nrow = 500, ncol = 209)
pred_c3plus <- matrix(string_compare(data$Var1, data$Var2, method = "cos", q = 3) / 100, nrow = 500, ncol = 209)
pred_n2plus <- matrix(string_compare(data$Var1, data$Var2, method = "jaccard", q = 2) / 100, nrow = 500, ncol = 209)
pred_n3plus <- matrix(string_compare(data$Var1, data$Var2, method = "jaccard", q = 3) / 100, nrow = 500, ncol = 209)
pred_lcsplus <- matrix(string_compare(data$Var1, data$Var2, method = "lcs") / 100, nrow = 500, ncol = 209)

pred_lv[is.na(pred_lv)] <- 0
pred_lcs[is.na(pred_lcs)] <- 0
pred_jw[is.na(pred_jw)] <- 0
pred_c2[is.na(pred_c2)] <- 0
pred_c3[is.na(pred_c3)] <- 0
pred_n2[is.na(pred_n2)] <- 0
pred_n3[is.na(pred_n3)] <- 0

pred_lvplus[is.na(pred_lvplus)] <- 0
pred_lcsplus[is.na(pred_lcsplus)] <- 0
pred_jwplus[is.na(pred_jwplus)] <- 0
pred_c2plus[is.na(pred_c2plus)] <- 0
pred_c3plus[is.na(pred_c3plus)] <- 0
pred_n2plus[is.na(pred_n2plus)] <- 0
pred_n3plus[is.na(pred_n3plus)] <- 0

output_lv <- predrec(pred_lv, truth)
output_lcs <- predrec(pred_lcs, truth)
output_jw <- predrec(pred_jw, truth)
output_c2 <- predrec(pred_c2, truth)
output_c3 <- predrec(pred_c3, truth)
output_n2 <- predrec(pred_n2, truth)
output_n3 <- predrec(pred_n3, truth)

output_lvplus <- predrec(pred_lvplus**2, truth)
output_lcsplus <- predrec(pred_lcsplus**2, truth)
output_jwplus <- predrec(pred_jwplus**2, truth)
output_c2plus <- predrec(pred_c2plus**2, truth)
output_c3plus <- predrec(pred_c3plus**2, truth)
output_n2plus <- predrec(pred_n2plus**2, truth)
output_n3plus <- predrec(pred_n3plus**2, truth)

p1 <- ggplot(output_lv, aes(x = recall, y = precision, colour = "a")) +
  geom_line(size = 1) +
  geom_line(data = output_lcs, aes(x = recall, y = precision, colour = "b"), size = 1) +
  geom_line(data = output_jw, aes(x = recall, y = precision, colour = "c"), size = 1) +
  geom_line(data = output_c2, aes(x = recall, y = precision, colour = "g"), size = 1) +
  geom_line(data = output_c3, aes(x = recall, y = precision, colour = "e"), size = 1) +
  geom_line(data = output_n2, aes(x = recall, y = precision, colour = "f"), size = 1) +
  geom_line(data = output_n3, aes(x = recall, y = precision, colour = "h"), size = 1) +
  scale_colour_manual(name = "method", 
                      values = c("#E7298A", "#E6AB02", "#7570B3", "#A6761D", "#666666", "#66A61E", "#D95F02"),
                      labels = c("levenshtein", "longest common substring", "jaro-winkler", "cosine 2-gram", "cosine 3-gram", "jaccard 2-gram", "jaccard 3-gram")) +
  theme_linedraw() +
  theme(legend.position = c(0.2, 0.3),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  guides(colour = guide_legend(override.aes = list(size = 3)))

png(filename = "./plots/figure_8_1_validation_common.png", width = 1024, height = 768, res = 150, type = "cairo")
p1
dev.off()

p2 <- ggplot(output_lvplus, aes(x = recall, y = precision, colour = "a")) +
  geom_line(size = 1) +
  geom_line(data = output_lv, aes(x = recall, y = precision), colour = "#E7298A", alpha = 0.8, size = 1, linetype="dashed") +
  geom_line(data = output_lcs, aes(x = recall, y = precision), colour = "#E6AB02", alpha = 0.8, size = 1, linetype="dashed") +
  geom_line(data = output_jw, aes(x = recall, y = precision), colour = "#7570B3", alpha = 0.8, size = 1, linetype="dashed") +
  geom_line(data = output_c2, aes(x = recall, y = precision), colour = "#A6761D", alpha = 0.8, size = 1, linetype="dashed") +
  geom_line(data = output_c3, aes(x = recall, y = precision), colour = "#666666", alpha = 0.8, size = 1, linetype="dashed") +
  geom_line(data = output_n2, aes(x = recall, y = precision), colour = "#66A61E", alpha = 0.8, size = 1, linetype="dashed") +
  geom_line(data = output_n3, aes(x = recall, y = precision), colour = "#D95F02", alpha = 0.8, size = 1, linetype="dashed") +
  geom_line(data = output_lcsplus, aes(x = recall, y = precision, colour = "b"), size = 1) +
  geom_line(data = output_jwplus, aes(x = recall, y = precision, colour = "c"), size = 1) +
  geom_line(data = output_c2plus, aes(x = recall, y = precision, colour = "g"), size = 1) +
  geom_line(data = output_c3plus, aes(x = recall, y = precision, colour = "e"), size = 1) +
  geom_line(data = output_n2plus, aes(x = recall, y = precision, colour = "f"), size = 1) +
  geom_line(data = output_n3plus, aes(x = recall, y = precision, colour = "h"), size = 1) +
  scale_colour_manual(name = "method", 
                      values = c("#E7298A", "#E6AB02", "#7570B3", "#A6761D", "#666666", "#66A61E", "#D95F02", "#E7298A", "#E6AB02", "#7570B3", "#A6761D", "#666666", "#66A61E", "#D95F02"),
                      labels = c("NS (levenshtein)", "NS (longest common substring)", "NS (jaro-winkler)", "NS (cosine 2-gram)", "NS (cosine 3-gram)", "NS (jaccard 2-gram)", "NS (jaccard 3-gram)", "levenshtein", "longest common substring", "jaro-winkler", "cosine 2-gram", "cosine 3-gram", "jaccard 2-gram", "jaccard 3-gram"),
                      limits = c("a", "b", "c", "e", "f", "g", "h")) +
  theme_linedraw() +
  theme(legend.position = c(0.2, 0.3),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  guides(colour = guide_legend(override.aes = list(size = 3)))

png(filename = "./plots/figure_8_1_validation_namesim.png", width = 1024, height = 768, res = 150, type = "cairo")
p2
dev.off()


# END OF SCRIPT