##############################.
#
#   Neuronsweep 0 hidden layers
#
#   Enzo Lima
#   Daniel Redhead
#   Subhajit Paul
#
##############################.

# Clear workspace ----
rm(list = ls())
library(tidyverse)
library(torch)
library(stringdist)
library(mgsub)

set.seed(421)
source("./R/functions.R", local = T)

# Load data ----
load("./data/output_long.rda")
output_long <- unique(output_long)
output_long <- output_long[nchar(output_long[,2]) > 6,]
output_long <- output_long[lengths(strsplit(output_long[,2], " ")) > 1,]
output_long <- output_long[c(1:400, 1601:2000, 3001:3400, 4401:4800, 6401:6800, 8801:9200, 20101:20500, 35000:42000),]
output_long <- output_long[sample(1:nrow(output_long), 1000),]

matched_names <- as.data.frame(output_long)[,c(2,1)]
colnames(matched_names) <- c("observation", "reference")
matched_names$match <- 1

# Creating dyads ----
non_matches <- matched_names[0,]
for(i in 1:nrow(matched_names)){
  add <- data.frame(observation = matched_names$observation[i], reference = matched_names$reference[matched_names$reference != matched_names$reference[i]], match = 0, stringsAsFactors = F)
  non_matches <- rbind(non_matches, add)
}

data <- rbind(matched_names, non_matches[sample(1:nrow(non_matches), 2000),])
data <- unique(data)
colnames(data) <- c("Var1", "Var2", "match")
cat("preprocessing : generating features \n")

# Calculating features ----
data$jaro = 1 - stringdist(data$Var1, data$Var2, method = "jw")
data$cos1 = 1 - stringdist(data$Var1, data$Var2, method = "cosine", q = 1)
data$cos2 = 1 - stringdist(data$Var1, data$Var2, method = "cosine", q = 2)
data$cos3 = 1 - stringdist(data$Var1, data$Var2, method = "cosine", q = 3)
data$init = initial_similarity(data$Var1, data$Var2)
data$lv <- 1 - stringdistnorm(data$Var1, data$Var2, method = "lv")
data$lcs <- 1 - stringdistnorm(data$Var1, data$Var2, method = "lcs") / 2
data$n2 <- 1 - stringdistnorm(data$Var1, data$Var2, method = "qgram", q = 2) / 2
data$n3 <- 1 - stringdistnorm(data$Var1, data$Var2, method = "qgram", q = 3) / 2
data$nj <- string_compare(data$Var1, data$Var2) / 100
data$phon <- phonetic_similarity(data$Var1, data$Var2)

data <- data[sample(1:nrow(data)),]

train_x <- data[1:1000,4:14]
train_y <- data[1:1000,3]

validate_x <- data[1001:2000,4:14]
validate_y <- data[1001:2000,3]

source("./scripts/2021_07_28_torch_dropout_nn_1d.R", local = T)
source("./scripts/2021_07_28_torch_dropout_pred_1d.R", local = T)

error_fn_list <- array(NA, dim = list(57, 100, 100))
error_fp_list <- array(NA, dim = list(57, 100, 100))
error_acc_list <- array(NA, dim = list(57, 100, 100))

# Generating accuracy data ----
for(k in 1:29){
  for(j in 1:100){
    error_fn <- c()
    error_fp <- c()
    error_acc <- c()
    model <- ann(train_x, train_y, neurons = 37 - k, learning_rate = 1e-9, ndropout = 0, iterations = 1)
    for(i in 1:100){
      model <- ann(train_x, train_y, neurons = 37 - k, learning_rate = 0.1, ndropout = 0, iterations = 10, model = model)
      pred <- round(ann_pred(validate_x, model, ndropout = 0)[[1]], 0)
      truth <- validate_y
      fn <- length(pred[pred == 0 & truth == 1])
      fp <- length(pred[pred == 1 & truth == 0])
      acc <- length(pred[pred != truth]) / 10
      
      error_acc_list[k, i, j] <- acc
      error_fn_list[k, i, j] <- fn
      error_fp_list[k, i, j] <- fp
    }
  }
}

# Saving statistics
save(error_acc_list, file = "./data/2021_08_07_neuronsweep_acc_36_8_1d.rda")
save(error_fn_list, file = "./data/2021_08_07_neuronsweep_fn_36_8_1d.rda")
save(error_fp_list, file = "./data/2021_08_07_neuronsweep_fp_36_8_1d.rda")


# END OF SCRIPT