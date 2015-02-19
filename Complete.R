complete <- function(directory,id = 1:332){
  ##pollutant <- "sulfate"  
  ##print(pollutant)
  idFrame <- data.frame("MonitorID" = id)
  ## print (id)
  dir <- getwd()
  ##print(dir)
  
  dir1 <- paste(dir, "specdata", sep="/")

  allFiles <- list.files(dir1)
  ##print(allFiles)
  fileRange <- allFiles[id]
  ##fileRange <- paste
  ##print (fileRange)

  fileList <- paste(dir1, fileRange, sep="/")
  
  ##print(check)
  nobs1 <- NULL
  id2<-0
  id3 = NULL
  nobs2 = NULL
  
  for(i in 1:length(id)){
    
    ##print(paste(dir1, fileRange, sep="/"))
    
    data <- read.csv(fileList[i])
    ##print (data)  
    ##print (fileRange[i])
    
    strip <- data[,c(2,3,4)]
    ##print(strip)
    sulfatenitrateonly <- strip[complete.cases(strip),]
    ##print(sulfatenitrateonly) 
    nobss <- nrow(sulfatenitrateonly)
    
    nobs1 <- append(nobs1, nobss)
    id2<-id2+1
    id3 <- append(id3, id2)
    
    
  }
 ## print (nobs1)
  ##print(id3)
  
 nobs2 <- data.frame("id"=id3, "nobs"=nobs1)
  return(nobs2)
}
    