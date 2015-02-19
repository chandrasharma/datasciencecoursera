#Write a function that reads a directory full of files and reports the number of completely observed
#cases in each data file. The function should return a data frame where the first column is the name of
#the file and the second column is the number of complete cases.

complete <- function(directory,id = 1:332){
 
  #Read individual files in a directory containing hundreds of excel files  
  idFrame <- data.frame("MonitorID" = id) 
  dir <- getwd()
  dir1 <- paste(dir, "specdata", sep="/")
  allFiles <- list.files(dir1)  
  fileRange <- allFiles[id]
  fileList <- paste(dir1, fileRange, sep="/")
  
  #initialize variables
  nobs1 <- NULL
  id2<-0
  id3 = NULL
  nobs2 = NULL
  
  
  for(i in 1:length(id)){

    data <- read.csv(fileList[i])
    strip <- data[,c(2,3,4)] 
    sulfatenitrateonly <- strip[complete.cases(strip),] 
    nobss <- nrow(sulfatenitrateonly)    
    nobs1 <- append(nobs1, nobss)
    id2<-id2+1
    id3 <- append(id3, id2)    
    
  }

  nobs2 <- data.frame("id"=id3, "nobs"=nobs1)
  return(nobs2)
}
    