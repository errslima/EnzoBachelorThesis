##############################.
#
#   Main model functions
#
#   Enzo Lima
#   Daniel Redhead
#   Subhajit Paul
#
##############################.

library(stringdist)
library(PGRdup)

stringdist_norm <- function(x, y, method, q = 1){
  normalize_var <- max(nchar(x), nchar(y))
  score <- stringdist(x, y, method = method, q = q)
  return(score / normalize_var)
}

stringdistnorm <- Vectorize(stringdist_norm)

init_similarity <- function(Var1, Var2){
  
  if(grepl("^[A-z ]+$", Var1) & grepl("^[A-z ]+$", Var2)){
    string1 <- substr(unlist(strsplit(Var1, " ")), 1, 1)
    string2 <- substr(unlist(strsplit(Var2, " ")), 1, 1)
    
    return((length(string1[string1 %in% string2])+length(string2[string2 %in% string1])) / (2*max(c(length(string1), length(string2)))))
  } else {
    return(0)
  }
}

initial_similarity <- Vectorize(init_similarity)

phon_similarity <- function(Var1, Var2){
  
  string1 <- paste(phonetic(unlist(strsplit(Var1, " "))), collapse = " ")
  string2 <- paste(phonetic(unlist(strsplit(Var2, " "))), collapse = " ")
  
  normalize_var <- max(nchar(string1), nchar(string2))
  score <- stringdist(string1, string2, method = "jw")
  
  return(score / normalize_var)
}

phonetic_similarity <- Vectorize(phon_similarity)

# The string.compare function assigns a match probability to two strings.
# Var1 is the first string
# Var2 is the second string

string.compare <- function(Var1, Var2, method = "jw", q = 1) {
  
  # the line below this one is for debugging purposes.
  # cat(paste(Var2, "and", Var1, "\n"))
  if(is.na(Var1) | is.na(Var2)){
    return(0)
  }
  if(nchar(Var1) == 0 | nchar(Var2) == 0){
    return(0)
  }
  # clean up both strings and split the individual words.
  string1 <- Var1 %>%
    tolower() %>%
    trimws() %>%
    str_replace_all(pattern = "[[:punct:]]", replacement = " ") %>%
    strsplit(" ") %>%
    unlist()
  string2 <- Var2 %>%
    tolower() %>%
    trimws() %>%
    str_replace_all(pattern = "[[:punct:]]", replacement = " ") %>%
    strsplit(" ") %>%
    unlist()
  
  # create a matrix whose dimensions are the number of words in each string.
  compare <- array(NA, dim = c(
    length(string1),
    length(string2)
  ), dimnames = list(
    string1,
    string2
  ))
  
  # assign a match score between each set of words.
  compare[] <- do.call(
    mapply,
    c(
      list(FUN = string.score, method = method, q = q),
      expand.grid(dimnames(compare), stringsAsFactors = FALSE)
    )
  )
  
  # determine which allowed order of matching words gives the highest total match score.
  max.sum <- string.max(compare)
  
  if(is.numeric(max.sum) & !is.nan(max.sum) & !is.na(max.sum) & !is.null(max.sum)){
    if (max.sum > 0) {
      
      # normalize the match score.
      score <- round(max.sum / sum(sapply(string1, function(x) string.score(x, x))) * 100, 3)
      return(score)
    } else {
      return(0)
    }
  } else {
    return(0)
  }
  
}

string_compare <- Vectorize(string.compare)


# The string.max function determines which allowed order of matching the words
# from two strings together gives the highest total match score.
# input is an mxn-matrix.

string.max <- function(input) {
  if (nrow(input) == 0 | ncol(input) == 0) {
    return(0)
  } else if (nrow(input) == 1 | ncol(input) == 1) {
    return(max(input))
  }
  
  permutations <- readRDS("./data/permutations.RDS")
  
  permutations <- substr(permutations, 1, nrow(input))
  permutations <- permutations[!grepl(paste0(c((ncol(input) + 1):9), collapse = "|"), permutations)]
  permutations <- unique(permutations)
  
  sums <- 0
  for (i in 1:length(permutations)) {
    add <- 0
    for (j in 1:nrow(input)) {
      if (!is.null(as.numeric(substr(permutations[i], j, j))) & !is.na(as.numeric(substr(permutations[i], j, j)))) {
        if (as.numeric(substr(permutations[i], j, j)) > 0) {
          add <- add + input[j, as.numeric(substr(permutations[i], j, j))]
        }
      }
    }
    if(!is.nan(add) & !is.null(add) & !is.na(add) & is.numeric(add) & length(add) == 1){
      sums <- max(c(sums, add))
    }
  }
  
  if(is.nan(sums) | is.na(sums) | is.null(sums) | !is.numeric(sums) | sums == 0){
    return(0)
  } else if (length(sums) > 0 & any(sums > 0)) {
    return(max(sums))
  } else {
    return(0)
  }
}


# The string.score function assigns a matching score to two words.
# Var1 is the first word.
# Var2 is the second word.

string.score <- function(Var1, Var2, method = "jw", q = 1) {
  
  if (is.null(Var1) | is.null(Var2) | is.na(Var1) | is.na(Var2) | Var1 == "" | Var2 == "") { # if one of the entries is empty, score 0
    return(0)
  } else if (Var1 == substr(Var2, 1, 1)) { # if Var1 is an abbreviation of Var2, score 50
    return(50)
  } else if (nchar(Var1) == 1) { # if Var1 is an abbreviation but not of Var2, score 0
    return(0)
  } else if (Var2 == substr(Var1, 1, 1)) { # if Var2 is an abbreviation of Var1, score 50
    return(50)
  } else if(nchar(Var2) == 1) {
    return(0)
  } else { # give a score based on the 1-gram cosine string distance
    if(method %in% c("jw", "cos", "jaccard")){
      return(round(100 - (100 * stringdist(Var1, Var2, method = method, q = q)), 3))
    } else if(method == "soundex"){
      return(round(100 - (100 * stringdist(phonetic(Var1), phonetic(Var2), method = "jw")), 3))
    } else if(method == "metaphone"){
      Phon1 <- DoubleMetaphone(Var1)
      Phon2 <- DoubleMetaphone(Var2)
      phonetic_score <- min(c(stringdist(Phon1$primary, Phon2$primary, method = "jw"),
                              stringdist(Phon1$primary, Phon2$alternate, method = "jw"),
                              stringdist(Phon1$alternate, Phon2$primary, method = "jw"),
                              stringdist(Phon1$alternate, Phon2$alternate, method = "jw")))
      return(round(100 - (100 * phonetic_score), 3))
    } else {
      return(round(100 - (100 * stringdistnorm(Var1, Var2, method = method)), 3))
    }
  }
}

predrec <- function(prediction, truth, invert = F){
  
  x <- c()
  y <- c()
  for(n in 0:99){
    pred <- prediction
    pred[pred < n/100] <- 0
    
    for(i in 1:nrow(pred)){
      if(length(pred[i,][pred[i,] == max(pred[i,])]) == 1){
        pred[i,][pred[i,] == max(pred[i,])] <- 1
        pred[i,][pred[i,] != max(pred[i,])] <- 0
      } else {
        pred[i,] <- 0
      }
    }
    
    ap <- length(truth[truth == 1])
    pp <- length(pred[pred == 1])
    tp <- length(pred[pred == 1 & truth == 1])
    
    x <- c(x, tp / ap)
    y <- c(y, tp / pp)
  }
  
  if(invert == T){
    return(data.frame(recall = 1-x, precision = 1-y, stringsAsFactors = F))
  } else {
    return(data.frame(recall = x, precision = y, stringsAsFactors = F))
  }
}

predrecsemi <- function(prediction, truth, invert = F){
  
  x <- c()
  y <- c()
  end <- ncol(prediction)
  for(n in 0:99){
    pred <- prediction
    pred[pred < n/100] <- 0
    pp <- 0
    for(i in 1:nrow(pred)){
      if(length(pred[i,][pred[i,] %in% sort(pred[i,], decreasing = T)[1:3]]) == 3){
        pred[i,][pred[i,] %in% sort(pred[i,], decreasing = T)[1:3]] <- 1
        pred[i,][pred[i,] %in% sort(pred[i,], decreasing = T)[4:end]] <- 0
        pp <- pp + 1
      } else if(length(pred[i,][pred[i,] == max(pred[i,])]) == 1){
        pred[i,][pred[i,] == max(pred[i,])] <- 1
        pred[i,][pred[i,] != max(pred[i,])] <- 0
        pp <- pp + 1
      } else {
        pred[i,] <- 0
      }
    }
    
    ap <- length(truth[truth == 1])
    tp <- length(pred[pred == 1 & truth == 1])
    
    x <- c(x, tp / ap)
    y <- c(y, tp / pp)
  }
  
  if(invert == T){
    return(data.frame(recall = 1-x, precision = 1-y, stringsAsFactors = F))
  } else {
    return(data.frame(recall = x, precision = y, stringsAsFactors = F))
  }
}