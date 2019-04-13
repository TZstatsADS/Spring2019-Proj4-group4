read_truth <- function(file_name){
  current_file_name <- sub(".txt","",file_name)
  current_ground_truth <- readLines(paste("../data/ground_truth/",current_file_name,".txt",sep=""), encoding="UTF-8",warn=FALSE)
  return(current_ground_truth)
}

read_ocr <- function(file_name){
  current_file_name <- sub(".txt","",file_name)
  current_ocr <- readLines(paste("../data/tesseract/",current_file_name,".txt",sep=""), encoding="UTF-8",warn=FALSE)
  return(current_ocr)
}