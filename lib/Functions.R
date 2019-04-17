##Get pairs of diff letters from two words

letterpair=function(error_word,dic_word)
{
  
  n=nchar(error_word)
  letter1=strsplit(error_word,split=NULL)%>%unlist()
  letter2=strsplit(dic_word,split=NULL)%>%unlist()
  ind=letter1!=letter2
  
  if(sum(ind)>2){return(NULL)}else{
    letterpair=cbind(letter1[ind],letter2[ind])
    colnames(letterpair)=c("mistake","true")
    rownames(letterpair)=rep(dic_word,sum(ind))
    return(letterpair)}
  
}




## Get product of Pr(ljf|ljs) 

product<-function(lettermat,prob){
  if(length(grep("[^a-z]",lettermat))>=1){return(NULL)}else{
    return(prod(prob[lettermat]))}
}





## Get probability of the word

wordfreq=function(topic_model,candidates,post_coef){
  word_freq=numeric(length(candidates))
  candidates%in%stop_words$word->stopinds
  candidates%in%dictionary->notstopinds
  
  dictionary%in%candidates->inds
  wordfreq_topic=topic_model@beta[,inds]%>%exp()
  
  word_freq[notstopinds]=post_coef %*% wordfreq_topic
  word_freq[stopinds]=1
  return(word_freq)
}





## Get best candidate for word error

bestCandidate=function(error_word,filenumber)
{
  n=nchar(error_word)
  
  sapply(dictionary_stop[nchar(dictionary_stop)==n],letterpair,error_word)%>%plyr::compact()->candidates
  
  lapply(candidates,product,prob)%>%plyr::compact()%>%unlist()->letter_prob #1x5
  topicProbabilities<-t(post_topics[filenumber,]) #1x5
  
  word_freq=wordfreq(mod_4, names(candidates),as.matrix(topicProbabilities))
  
  scores=word_freq*letter_prob
  best_candidate<-names(candidates)[which.max(scores)]
  return(best_candidate)
}




## Replace word error

replace_error=function(error_vector,filenumber)
{
  sapply(error_vector,bestCandidate,filenumber)->postcorrection
  return(postcorrection)
}

replace_all_file=function(error_list){
  mapply(FUN=replace_error,error_list,filenumber=1:length(error_list))
}

