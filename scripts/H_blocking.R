

# Clear workspace 
rm(list = ls())

# Load functions
library(stringdist)
source("./R/functions.R")

# Load data
load("./test_data/testdata_simulated.rda")
# Select 1000 matches
matched_names <- matched_names[sample(1:nrow(matched_names), 1000),]

# Generate all dyads
data <- expand.grid(unique(matched_names$nomination), unique(matched_names$match), stringsAsFactors = F)
data$match <- 0

# Assign "1" to all dyads that represent a match
for(i in 1:nrow(matched_names)){
  data$match[data$Var1 == matched_names$nomination[i] & data$Var2 == matched_names$match[i]] <- 1
  if(i %% 10 == 0){
    cat(paste0(i/1000, "\n"))
  }
}

n_m <- 1000 # How many samples to take of the dyads?

data <- data[sample(1:nrow(data), n_m),]

data$Var1 -> nomination
data$Var2 -> reference

n_i <- 10  # how many iterations to determine the speed of each feature?
speed <- data.frame(feature = NA, speed = NA, stringsAsFactors = F)[0,]

# Generating advanced features JW ----
t <- 0
for(i in 1:n_i){
  start <- Sys.time()
  data$jw = 1 - stringdist(nomination, reference, method = "jw")
  end <- Sys.time()
  cat("JW \n")
  t <- t + as.numeric(as.numeric(difftime(end, start, units = "secs")))
}
speed <- rbind(speed, data.frame(feature = "JW", speed = n_m * n_i / t, stringsAsFactors = F))

t <- 0
for(i in 1:n_i){
  start <- Sys.time()
  data$jwplus = string_compare(nomination, reference, method = "jw") / 100
  end <- Sys.time()
  cat("\n JW+ \n")
  t <- t + as.numeric(difftime(end, start, units = "secs"))
}
speed <- rbind(speed, data.frame(feature = "JW+", speed = n_m * n_i / t, stringsAsFactors = F))
# Generating advanced features LV ----
t <- 0
for(i in 1:n_i){
  start <- Sys.time()
  data$lv = 1 - stringdistnorm(nomination, reference, method = "lv")
  end <- Sys.time()
  cat("\n LV \n")
  t <- t + as.numeric(difftime(end, start, units = "secs"))
}
speed <- rbind(speed, data.frame(feature = "LV", speed = n_m * n_i / t, stringsAsFactors = F))

t <- 0
for(i in 1:n_i){
  start <- Sys.time()
  data$lvplus = string_compare(nomination, reference, method = "lv") / 100
  end <- Sys.time()
  cat("\n LV+ \n")
  t <- t + as.numeric(difftime(end, start, units = "secs"))
}
speed <- rbind(speed, data.frame(feature = "LV+", speed = n_m * n_i / t, stringsAsFactors = F))
# Generating advanced features LCS ----
t <- 0
for(i in 1:n_i){
  start <- Sys.time()
  data$lcs = 1 - stringdistnorm(nomination, reference, method = "lcs") / 2
  end <- Sys.time()
  cat("\n LCS \n")
  t <- t + as.numeric(difftime(end, start, units = "secs"))
}
speed <- rbind(speed, data.frame(feature = "LCS", speed = n_m * n_i / t, stringsAsFactors = F))

t <- 0
for(i in 1:n_i){
  start <- Sys.time()
  data$lcsplus = string_compare(nomination, reference, method = "lcs") / 100
  end <- Sys.time()
  cat("\n LCS+ \n")
  t <- t + as.numeric(difftime(end, start, units = "secs"))
}
speed <- rbind(speed, data.frame(feature = "LCS+", speed = n_m * n_i / t, stringsAsFactors = F))

# Generating advanced features 1-GRAM ----
t <- 0
for(i in 1:n_i){
  start <- Sys.time()
  data$cos1 = 1 - stringdist(nomination, reference, method = "cos", q = 1)
  end <- Sys.time()
  cat("\n COS1 \n")
  t <- t + as.numeric(difftime(end, start, units = "secs"))
}
speed <- rbind(speed, data.frame(feature = "COS1", speed = n_m * n_i / t, stringsAsFactors = F))

t <- 0
for(i in 1:n_i){
  start <- Sys.time()
  data$cos1plus = string_compare(nomination, reference, method = "cos", q = 1) / 100
  end <- Sys.time()
  cat("\n COS1+ \n")
  t <- t + as.numeric(difftime(end, start, units = "secs"))
}
speed <- rbind(speed, data.frame(feature = "COS1+", speed = n_m * n_i / t, stringsAsFactors = F))

# Generating advanced features 2-GRAM ----
t <- 0
for(i in 1:n_i){
  start <- Sys.time()
  data$cos2 = 1 - stringdist(nomination, reference, method = "cos", q = 2)
  end <- Sys.time()
  cat("\n COS2 \n")
  t <- t + as.numeric(difftime(end, start, units = "secs"))
}
speed <- rbind(speed, data.frame(feature = "COS2", speed = n_m * n_i / t, stringsAsFactors = F))

t <- 0
for(i in 1:n_i){
  start <- Sys.time()
  data$cos2plus = string_compare(nomination, reference, method = "cos", q = 2) / 100
  end <- Sys.time()
  cat("\n COS2+ \n")
  t <- t + as.numeric(difftime(end, start, units = "secs"))
}
speed <- rbind(speed, data.frame(feature = "COS2+", speed = n_m * n_i / t, stringsAsFactors = F))

# Generating advanced features 3-GRAM ----
t <- 0
for(i in 1:n_i){
  start <- Sys.time()
  data$cos3 = 1 - stringdist(nomination, reference, method = "cos", q = 3)
  end <- Sys.time()
  cat("\n COS3 \n")
  t <- t + as.numeric(difftime(end, start, units = "secs"))
}
speed <- rbind(speed, data.frame(feature = "COS3", speed = n_m * n_i / t, stringsAsFactors = F))

t <- 0
for(i in 1:n_i){
  start <- Sys.time()
  data$cos3plus = string_compare(nomination, reference, method = "cos", q = 3) / 100
  end <- Sys.time()
  cat("\n COS3+ \n")
  t <- t + as.numeric(difftime(end, start, units = "secs"))
}
speed <- rbind(speed, data.frame(feature = "COS3+", speed = n_m * n_i / t, stringsAsFactors = F))

# Generating advanced features 1-GRAM ----
t <- 0
for(i in 1:n_i){
  start <- Sys.time()
  data$n1 = 1 - stringdistnorm(nomination, reference, method = "qgram", q = 1) / 2
  end <- Sys.time()
  cat("\n N1 \n")
  t <- t + as.numeric(difftime(end, start, units = "secs"))
}
speed <- rbind(speed, data.frame(feature = "N1", speed = n_m * n_i / t, stringsAsFactors = F))

t <- 0
for(i in 1:n_i){
  start <- Sys.time()
  data$n1plus = string_compare(nomination, reference, method = "qgram", q = 1) / 100
  end <- Sys.time()
  cat("\n N1+ \n")
  t <- t + as.numeric(difftime(end, start, units = "secs"))
}
speed <- rbind(speed, data.frame(feature = "N1+", speed = n_m * n_i / t, stringsAsFactors = F))

# Generating advanced features 2-GRAM ----
t <- 0
for(i in 1:n_i){
  start <- Sys.time()
  data$n2 = 1 - stringdistnorm(nomination, reference, method = "qgram", q = 2) / 2
  end <- Sys.time()
  cat("\n N2 \n")
  t <- t + as.numeric(difftime(end, start, units = "secs"))
}
speed <- rbind(speed, data.frame(feature = "N2", speed = n_m * n_i / t, stringsAsFactors = F))

t <- 0
for(i in 1:n_i){
  start <- Sys.time()
  data$n2plus = string_compare(nomination, reference, method = "qgram", q = 2) / 100
  end <- Sys.time()
  cat("\n N2+ \n")
  t <- t + as.numeric(difftime(end, start, units = "secs"))
}
speed <- rbind(speed, data.frame(feature = "N2+", speed = n_m * n_i / t, stringsAsFactors = F))

# Generating advanced features 3-GRAM ----
t <- 0
for(i in 1:n_i){
  start <- Sys.time()
  data$n3 = 1 - stringdistnorm(nomination, reference, method = "qgram", q = 3) / 2
  end <- Sys.time()
  cat("\n N3 \n")
  t <- t + as.numeric(difftime(end, start, units = "secs"))
}
speed <- rbind(speed, data.frame(feature = "N3", speed = n_m * n_i / t, stringsAsFactors = F))

t <- 0
for(i in 1:n_i){
  start <- Sys.time()
  data$n3plus = string_compare(nomination, reference, method = "qgram", q = 3) / 100
  end <- Sys.time()
  cat("\n N3+ \n")
  t <- t + as.numeric(difftime(end, start, units = "secs"))
}
speed <- rbind(speed, data.frame(feature = "N3+", speed = n_m * n_i / t, stringsAsFactors = F))

# Generating advanced features 2-GRAM ----
t <- 0
for(i in 1:n_i){
  start <- Sys.time()
  data$jac1 = 1 - stringdist(nomination, reference, method = "jaccard", q = 1)
  end <- Sys.time()
  cat("\n JAC1 \n")
  t <- t + as.numeric(difftime(end, start, units = "secs"))
}
speed <- rbind(speed, data.frame(feature = "JAC1", speed = n_m * n_i / t, stringsAsFactors = F))

t <- 0
for(i in 1:n_i){
  start <- Sys.time()
  data$jac1plus = string_compare(nomination, reference, method = "jaccard", q = 1) / 100
  end <- Sys.time()
  cat("\n JAC1+ \n")
  t <- t + as.numeric(difftime(end, start, units = "secs"))
}
speed <- rbind(speed, data.frame(feature = "JAC1+", speed = n_m * n_i / t, stringsAsFactors = F))

# Generating advanced features 2-GRAM ----
t <- 0
for(i in 1:n_i){
  start <- Sys.time()
  data$jac2 = 1 - stringdist(nomination, reference, method = "jaccard", q = 2)
  end <- Sys.time()
  cat("\n JAC2 \n")
  t <- t + as.numeric(difftime(end, start, units = "secs"))
}
speed <- rbind(speed, data.frame(feature = "JAC2", speed = n_m * n_i / t, stringsAsFactors = F))

t <- 0
for(i in 1:n_i){
  start <- Sys.time()
  data$jac2plus = string_compare(nomination, reference, method = "jaccard", q = 2) / 100
  end <- Sys.time()
  cat("\n JAC2+ \n")
  t <- t + as.numeric(difftime(end, start, units = "secs"))
}
speed <- rbind(speed, data.frame(feature = "JAC2+", speed = n_m * n_i / t, stringsAsFactors = F))

# Generating advanced features 3-GRAM ----
t <- 0
for(i in 1:n_i){
  start <- Sys.time()
  data$jac3 = 1 - stringdist(nomination, reference, method = "jaccard", q = 3)
  end <- Sys.time()
  cat("\n JAC3 \n")
  t <- t + as.numeric(difftime(end, start, units = "secs"))
}
speed <- rbind(speed, data.frame(feature = "JAC3", speed = n_m * n_i / t, stringsAsFactors = F))

t <- 0
for(i in 1:n_i){
  start <- Sys.time()
  data$jac3plus = string_compare(nomination, reference, method = "jaccard", q = 3) / 100
  end <- Sys.time()
  cat("\n JAC3+ \n")
  t <- t + as.numeric(difftime(end, start, units = "secs"))
}
speed <- rbind(speed, data.frame(feature = "JAC3+", speed = n_m * n_i / t, stringsAsFactors = F))

# Generating advanced features INIT ----
t <- 0
for(i in 1:n_i){
  start <- Sys.time()
  data$init = initial_similarity(nomination, reference)
  end <- Sys.time()
  cat("\n INIT \n")
  t <- t + as.numeric(difftime(end, start, units = "secs"))
}
speed <- rbind(speed, data.frame(feature = "INIT", speed = n_m * n_i / t, stringsAsFactors = F))

# Generating advanced features PHONETIC ----
t <- 0
for(i in 1:n_i){
  start <- Sys.time()
  data$phon = phonetic_similarity(nomination, reference)
  end <- Sys.time()
  cat("\n PHON \n")
  t <- t + as.numeric(difftime(end, start, units = "secs"))
}
speed <- rbind(speed, data.frame(feature = "PHON", speed = n_m * n_i / t, stringsAsFactors = F))

t <- 0
for(i in 1:n_i){
  start <- Sys.time()
  data$phonplus = string_compare(nomination, reference, method = "soundex") / 100
  end <- Sys.time()
  cat("\n PHON+ \n")
  t <- t + as.numeric(difftime(end, start, units = "secs"))
}

# This data frame contains the speed of each feature in dyads / second
speed <- rbind(speed, data.frame(feature = "PHON+", speed = n_m * n_i / t, stringsAsFactors = F))

# Separate matches from non-matches
data_pos <- data[data$match == 1,]
data_neg <- data[data$match == 0,]

# Find the lowest value of each feature for the matches, then find what amount of non-matches fall below that value
feature_blocking <- data.frame(feature = NA, min = NA, max = NA, blocking = NA, stringsAsFactors = F)[0,]
for(i in 4:29){
  feature <- colnames(data_pos)[i]
  min <- min(data_pos[,i])
  max <- max(data_pos[,i])
  blocking <- length(data_neg$match[data_neg[,i] < min]) / nrow(data_neg)
  add <- data.frame(feature = feature, min = min, max = max, blocking = blocking, stringsAsFactors = F)
  feature_blocking <- rbind(feature_blocking, add)
}

save(feature_blocking, file = "./data/2021_08_22_feature_blocking_table.rda")


# END OF SCRIPT