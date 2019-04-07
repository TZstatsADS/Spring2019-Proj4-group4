if (!require("NLP")) install.packages("NLP")
if (!require("stringr")) install.packages("stringr")
library(NLP)
library(stringr)


# token = "Wrldfoooiaehr70).'"

bigram <- function(token){
  ## Split into characterss:
  w <- strsplit(token, "")[[1]]
  ## Word tri-grams pasted together:
  return(vapply(ngrams(w, 2L), paste, "", collapse = ""))
}


freq_bigr = function(bigr, LB){
  freq = LB$Freq[bigr == LB$Lb]
  return(ifelse(length(freq)!=0, freq, 0))
}


#x <- c("ac", "ad", "oo","ae","r7","ii")
#y <- c(3,4,6,7,9,2)
#LB <- data.frame(word =x ,freq = y )


findfeatures <- function(token){
  features <- list()
  #### feature 1
  l <- nchar(token)
  features[[1]] <- l
  
  #### feature 2
  vowel <- "[aeiouAEIOU]" # vowels
  consonants <- "[bcdfghjklmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ]" # consonants
  v <- str_count(token, pattern = vowel) # number of vowels
  c <- str_count(token, pattern = consonants) # number of consonants
  features[[2]] <- c(v/l, c/l, v/(c+0.1)) # v/(c+0.1): avoid NULL and Inf values
  
  ### feature 3
  ln <- "[:alnum:]" # letters and numbers
  s <- l - str_count(token, pattern = ln)
  features[[3]] <- s/l
  
  ### feature 4
  digit <- "[:digit:]"
  d <- str_count(token, pattern = digit)
  features[[4]] <- d/l
  
  ### feature 5
  upr <- "[:upper:]"
  lwr <- "[:lower:]"
  upp <- str_count(token, pattern = upr)
  low <- str_count(token, pattern = lwr) 
  features[[5]] <- c(low/l, upp/l)
  
  ### feature 6
  letters <- strsplit(token, "")[[1]]
  run_length <- max(rle(letters)$length)
  m <- ifelse(run_length >= 3, run_length/l,0)
  features[[6]] <- m
  
  ### feature 7
  la <- str_count(token, pattern = ln) 
  features[[7]] <- ifelse(s > la, 1,0) # k = s
  
  ### feature 8
  c_token <- gsub(consonants, "c", token) # replace the consonants with letter c
  c_split <- strsplit(c_token, "")[[1]]
  c_run_length <- max(rle(c_split)$length)
  features[[8]] <- ifelse(c_run_length >=6, 1,0)
  
  ### feature 9
  infix <- substr(token, start = 2, stop = nchar(token)-1)
  n <- l - str_count(infix, ln)
  features[[9]] <- ifelse(n >= 2, 1,0)
  
  ### feature 10 -- Bigram: the "naturalness of bigram"
  nums <- nchar(token)-1
  bigrs <- tolower(bigram(token)) # change into lowercase
  features[[10]] <- (sum(unlist(lapply(bigrs, freq_bigr, LB)))/10000)/nums
  
  ### return features
  return(unlist(features))
}

# findfeatures(token)




