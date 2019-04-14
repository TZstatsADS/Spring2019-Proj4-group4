if (!require("NLP")) install.packages("NLP")
if (!require("stringr")) install.packages("stringr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringdist")) install.packages("stringdist")
library(NLP)
library(stringr)
library(dplyr)
library(stringdist)


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



#### feature 1
feat1 <- function(token){
  return(nchar(token))
}

#### feature 2
feat2 <- function(token){
  vowel <- "[aeiouAEIOU]" # vowels
  consonants <- "[bcdfghjklmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ]" # consonants
  v <- str_count(token, pattern = vowel) # number of vowels
  c <- str_count(token, pattern = consonants) # number of consonants
  l <- nchar(token)
  return(c(v/l, c/l, v/(c+0.1)))# v/(c+0.1): avoid NULL and Inf values
}
  
### feature 3
feat3 <- function(token){
  ln <- "[:alnum:]" # letters and numbers
  s <- nchar(token) - str_count(token, pattern = ln)
  return(s/nchar(token))
}
  
### feature 4
feat4 <- function(token){
  digit <- "[:digit:]"
  d <- str_count(token, pattern = digit)
  return(d/nchar(token))
}
  
### feature 5
feat5 <- function(token){
  upr <- "[:upper:]"
  lwr <- "[:lower:]"
  upp <- str_count(token, pattern = upr)
  low <- str_count(token, pattern = lwr) 
  return(c(low/nchar(token), upp/nchar(token)))
}

### feature 6
feat6 <- function(token){
  letters <- strsplit(token, "")[[1]]
  run_length <- max(rle(letters)$length)
  m <- ifelse(run_length >= 3, run_length/nchar(token),0)
  return(m)
}
  


 
### feature 7
feat7 <- function(token){
  ln <- "[:alnum:]" # letters and numbers
  k <- nchar(token) - str_count(token, pattern = ln)
  la <- str_count(token, pattern = ln) 
  return(ifelse(k > la, 1,0))
}
  
  
### feature 8
feat8 <- function(token){
  consonants <- "[bcdfghjklmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ]" # consonants
  c_token <- gsub(consonants, "c", token) # replace the consonants with letter c
  c_split <- strsplit(c_token, "")[[1]]
  c_run_length <- max(rle(c_split)$length)
  return(ifelse(c_run_length >=6, 1,0))
}

### feature 9
feat9 <- function(token){
  ln <- "[:alnum:]"
  infix <- substr(token, start = 2, stop = nchar(token)-1)
  n <- nchar(token) - str_count(infix, ln)
  return(ifelse(n >= 2, 1,0))
}

  
### feature 10 -- Bigram: the "naturalness of bigram"
feat10 <- function(token){
  nums <- nchar(token)-1
  bigrs <- tolower(bigram(token)) # change into lowercase
  return((sum(unlist(lapply(bigrs, freq_bigr, LB)))/10000)/nums)
}

### feature 11 -- Most frequent symbol
feat11 <- function(token){
  i <- strsplit(token, "")[[1]] %>% tolower() %>% table() %>% max()
  return(ifelse( i<3, 0, i))
}

### feature 12 -- Non-alphabetical symbols
feat12 <- function(token){
  l <- nchar(token)
  l1 <- str_count(token, pattern = "[:alpha:]")
  return(ifelse(l-l1>0, l1/(l-l1),100))
}


### feature 13 -- Levenshtein distance
feat13 <- function(token){
  l <- nchar(token)
  lv <- stringdist(tolower(token),lexicon,method='lv') 
  v <- min(lv)
  l_adist <- token %>% adist(., lexicon[which.min(lv)], counts = T) 
  p <- attr(l_adist, "counts")[,,3]  %>% as.vector()
  return(ifelse(v > 2, l, (v + p/2 + 1)/l))
}

  
  
  ### return features
  return(unlist(features))

