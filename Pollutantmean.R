pollutantmean <- function(directory, pollutant, id = 1:332){

dir <- getwd()
dir1 <- paste(dir, "specdata", sep="/")
allFiles <- list.files(dir1) 
fileRange <- allFiles[id]
fileList <- paste(dir1, fileRange, sep="/")
  
print(fileList)

                sulfateCur<-NULL
                sulfate.apend<-NULL
                nitrateCur<-NULL
                nitrate.apend<-NULL
                nitrateValues<-NULL
                collection.V <- NULL

len<-length(id)
   
  for(i in 1:len){

    data <- read.csv(fileList[i])
    strip <- data[,c(2,3,4)]
    sulfatenitrateonly <- strip[complete.cases(strip),]

          if(pollutant == "sulfate"){
                
            sulfateCur<- (sulfatenitrateonly[,1])
            sulfate.apend <- append(sulfate.apend, sulfateCur)
            collection.V<-sulfate.apend
            
              }else{ 
              if(pollutant == "nitrate"){
              
                nitrateCur<- mean (sulfatenitrateonly[,2])

                nitrate.apend <- append(nitrate.apend, nitrateCur)
                collection.V <- nitrate.apend 

                    } else {
                     print("first arguement must be either sulfate or nitrate")
                       }
              output <- mean (collection.V)
                    }
output <- mean (collection.V)
    }

return (output)

  } 
