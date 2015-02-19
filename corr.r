corr <- function(directory, threshold=0){
  id <- 1:332
  ##idFrame <- data.frame("MonitorID" = id)  
  dir <- getwd()  
  dir1 <- paste(dir, "specdata", sep="/")  
  allFiles <- list.files(dir1)   
  fileRange <- allFiles[id]
  fileList <- paste(dir1, fileRange, sep="/")
  
  ##print (fileList)
   
  nobs1 <- NULL
  id2<-0
  id3<-NULL
  nobs2<- NULL
  compCasesV<-NULL
  corrol <- NULL
  corro <- NULL
  corrsNum <- numeric(0)
  ncobs.lst<-NULL
  
  test<-0
  
  for(i in id){
    
    data <- read.csv(fileList[i])
##print (data)
    strip <- data[,c(2,3,4)]

    sulfateOnly <- data.frame("sulfate"=data[c(2)])
    nitrateOnly <- data.frame("nitrate"=data[c(3)])
    newdataframe<-cbind(nitrateOnly,sulfateOnly)
    ncobs<-sum(complete.cases(newdataframe))

    ncobs.lst<-append(ncobs.lst, ncobs)

        if(missing(threshold)){

          corro <- cor(sulfateOnly,nitrateOnly,use="pairwise.complete.obs")          
          corrol <- (append(corrol, corro))
          corrol<-round(corrol,5)

        }

        if(ncobs > threshold){

        corro <- cor(sulfateOnly, nitrateOnly,use="pairwise.complete.obs")
        corrol <- (append(corrol, corro))
        corrol<-round(corrol,5)
        }else{
          if(max(ncobs.lst) < threshold){
            
            corrol<-numeric()
          }
          
        } 

    
  }

 return(corrol)

}
    