#Write a function that takes a directory of data files and a threshold for complete cases and calculates
#the correlation between sulfate and nitrate for monitor locations where the number of completely
#observed cases (on all variables) is greater than the threshold. The function should return a vector of
#correlations for the monitors that meet the threshold requirement. If no monitors meet the threshold
#requirement, then the function should return a numeric vector of length 0.

corr <- function(directory, threshold=0){
  
  id <- 1:332
  
  dir <- getwd()  
  dir1 <- paste(dir, "specdata", sep="/")  
  allFiles <- list.files(dir1)   
  fileRange <- allFiles[id]
  fileList <- paste(dir1, fileRange, sep="/")

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
    