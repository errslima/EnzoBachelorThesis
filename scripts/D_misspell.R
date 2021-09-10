# x = name spelled in latin characters
# y = name spelled in IPA (international phonetic alphabet)
# abbr = probability that one of the names gets abbreviated, (first, middle, last)
# miss = probability that one of the names gets deleted, (first, middle, last)
# phon = probability that one of the names is phonetically spelled, (first, middle, last)
misspell <- function(x, y, abbr = c(0.3, 0.1, 0.2), miss = c(0.1,0.6,0.1), phon = c(0.9,0.7,0.5), language = "english"){
  
  x <- as.character(x)
  y <- as.character(y)
  
  x <- unlist(strsplit(x, " "))
  phonetic <- unlist(strsplit(y, " "))
  
  if(length(x) == 1){
    
    abbr <- abbr[1]
    miss <- miss[1]
    phon <- phon[1]
    
    if(sample(c(TRUE, FALSE), size = 1, prob = c(abbr, 1 - abbr))){
      x <- substring(x, 1, 1)
    }
    
    if(nchar(x) != 1 & sample(c(TRUE, FALSE), size = 1, prob = c(miss, 1 - miss))){
      x <- ""
    }
    
    if(nchar(x) != 1 & sample(c(TRUE, FALSE), size = 1, prob = c(phon, 1 - phon))){
      x <- phonetic_write(phonetic, language = language)
    }
    
    if(nchar(x) != 1 & sample(c(TRUE, FALSE), size = 1, prob = c(phon, 1 - phon))){
      x <- edit_distance(x)
    }
    
  } else if(length(x) == 2){
    if(sample(c(TRUE, FALSE), size = 1, prob = c(abbr[1], 1 - abbr[1]))){
      x[1] <- substring(x[1], 1, 1)
    } else if(sample(c(TRUE, FALSE), size = 1, prob = c(abbr[length(abbr)], 1 - abbr[length(abbr)]))){
      x[2] <- substring(x[2], 1, 1)
    }
    
    if(nchar(x[1]) != 1 & sample(c(TRUE, FALSE), size = 1, prob = c(miss[1], 1 - miss[1]))){
      x[1] <- ""
    } else if(nchar(x[2]) != 1 & sample(c(TRUE, FALSE), size = 1, prob = c(miss[length(miss)], 1 - miss[length(miss)]))){
      x[2] <- ""
    }
    
    if(nchar(x[1]) != 1 & sample(c(TRUE, FALSE), size = 1, prob = c(phon[1], 1 - phon[1]))){
      x[1] <- phonetic_write(phonetic[1], language = language)
    }
    
    if(nchar(x[1]) != 1 & sample(c(TRUE, FALSE), size = 1, prob = c(phon[length(phon)], 1 - phon[length(phon)]))){
      x[1] <- edit_distance(x[1])
    }
    
    if(nchar(x[2]) != 1 & sample(c(TRUE, FALSE), size = 1, prob = c(phon[length(phon)], 1 - phon[length(phon)]))){
      x[2] <- phonetic_write(phonetic[2], language = language)
    }
    
    if(nchar(x[2]) != 1 & sample(c(TRUE, FALSE), size = 1, prob = c(phon[length(phon)], 1 - phon[length(phon)]))){
      x[2] <- edit_distance(x[2])
    }
    
    x <- x[!is.na(x) & x != ""]
    x <- paste(x, collapse = " ")
    
  } else if(length(x) > 2){
    if(sample(c(TRUE, FALSE), size = 1, prob = c(abbr[1], 1 - abbr[1]))){
      x[1] <- substring(x[1], 1, 1)
    } else if(sample(c(TRUE, FALSE), size = 1, prob = c(abbr[2], 1 - abbr[2]))){
      x[2:(length(x)-1)] <- substring(x[2:(length(x)-1)], 1, 1)
    } else if(sample(c(TRUE, FALSE), size = 1, prob = c(abbr[3], 1 - abbr[3]))){
      x[length(x)] <- substring(x[length(x)], 1, 1)
    }
    
    if(nchar(x[1]) != 1 & sample(c(TRUE, FALSE), size = 1, prob = c(miss[1], 1 - miss[1]))){
      x[1] <- ""
    } else if(sample(c(TRUE, FALSE), size = 1, prob = c(miss[2], 1 - miss[2]))){
      x[2:(length(x)-1)] <- ""
    } else if(nchar(x[length(x)]) != 1 & sample(c(TRUE, FALSE), size = 1, prob = c(miss[3], 1 - miss[3]))){
      x[length(x)] <- ""
    }
    
    if(nchar(x[1]) != 1 & sample(c(TRUE, FALSE), size = 1, prob = c(phon[1], 1 - phon[1]))){
      x[1] <- phonetic_write(phonetic[1], language = language)
    }
    
    if(nchar(x[1]) != 1 & sample(c(TRUE, FALSE), size = 1, prob = c(phon[1], 1 - phon[1]))){
      x[1] <- edit_distance(x[1])
    }
    
    if(sample(c(TRUE, FALSE), size = 1, prob = c(phon[2], 1 - phon[2]))){
      x[2:(length(x)-1)] <- phonetic_write(phonetic[x[2:(length(x)-1)]], language = language)
    }
    
    if(sample(c(TRUE, FALSE), size = 1, prob = c(phon[2], 1 - phon[2]))){
      for(p in 2:(length(x)-1))
      x[p] <- edit_distance(x[p])
    }
    
    if(nchar(x[length(x)]) != 1 & sample(c(TRUE, FALSE), size = 1, prob = c(phon[3], 1 - phon[3]))){
      x[length(x)] <- phonetic_write(phonetic[length(x)], language = language)
    }
    
    if(nchar(x[length(x)]) != 1 & sample(c(TRUE, FALSE), size = 1, prob = c(phon[3], 1 - phon[3]))){
      x[length(x)] <- edit_distance(x[length(x)])
    }
    
    x <- x[!is.na(x) & x != ""]
    x <- paste(x, collapse = " ")
  }
  
  return(x)
}

# This function attempts to write the input string in a phonetic way
phonetic_write <- function(x, language = "english"){
  
  x <- as.character(x)
  load("./data/ipa_reverse_enzo.rda")
  ipa_reverse <- ipa_reverse[order(nchar(ipa_reverse[,1]), decreasing = T),]
  
  for(i in 1:nrow(ipa_reverse)){
    if(ipa_reverse[i,language] != ""){
      replace <- sample(unlist(strsplit(ipa_reverse[i,language], "\\.")), 1)
      x <- gsub(ipa_reverse[i,1], replace, x)
    } else {
      x <- gsub(ipa_reverse[i,1], "", x)
    }
  }
  return(x)
}

# ins = probability for insertion of random character
# del = probability of deleting a random character from the string
# sub = probability of subsituting a random character from the string by a random other character
# tra = probability of transposing two characters
edit_distance <- function(x, ins = 0.005, del = 0.05, sub = 0.001, tra = 0){
  
  x <- as.character(x)
  
  if(is.na(x) | is.null(x) | length(x) < 1){
    return("")
  }
  
  if(nchar(x) < 2){
    return(x)
  }
  
  x <- unlist(strsplit(x, ""))
  
  for(i in 2:length(x)){
    
    if(sample(c(TRUE, FALSE), size = 1, prob = c(ins, 1 - ins))){
      x <- c(x[1:(i-1)], sample(letters, 1), x[i:length(x)])
    }
    
    if(sample(c(TRUE, FALSE), size = 1, prob = c(del, 1 - del))){
      x <- x[-i]
    }
    
    if(sample(c(TRUE, FALSE), size = 1, prob = c(sub, 1 - sub))){
      x <- x[-i]
      x <- c(x[1:(i-1)], sample(letters, 1), x[i:length(x)])
    }
    
    if(sample(c(TRUE, FALSE), size = 1, prob = c(tra, 1 - tra))){
      t1 <- x[i-1]
      t2 <- x[i]
      x[i-1] <- t2
      x[i] <- t1
    }
  }
  x <- x[!is.na(x)]
  x <- paste0(x, collapse = "")
  return(x)
}


# END OF SCRIPT