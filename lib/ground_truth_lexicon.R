stringsplit <-function(line){
  return(strsplit(line, " ")[[1]])
}
file_name_vec <- list.files("../data/ground_truth") 
ground_truth_list <- list()
for (i in c(1:length(file_name_vec))){
  current_file_name <- file_name_vec[i]
  ## read the ground truth text
  current_ground_truth_txt <- readLines(paste("../data/ground_truth/",current_file_name,sep=""), warn=FALSE)
  current_ground_truth_list_vec <- lapply(current_ground_truth_txt, stringsplit)
  ground_truth_list[[i]] <- unlist(current_ground_truth_list_vec)
}
ground_truth_lexicon <- unlist(ground_truth_list)
save(ground_truth_lexicon, file = "../output/ground_truth_lexicon.RData")