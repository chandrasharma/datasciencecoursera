#Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate)
#across a specified list of monitors. The function 'pollutantmean' takes three arguments: 'directory',
#'pollutant', and 'id'. Given a vector monitor ID numbers, 'pollutantmean' reads that monitors'
#particulate matter data from the directory specified in the 'directory' argument and returns the mean of
#the pollutant across all of the monitors, ignoring any missing values coded as NA.

pollutantmean <- function(directory, pollutant, id = 1:332){

dir <- getwd()
dir1 <- paste(dir, "specdata", sep="/")
allFiles <- list.files(dir1) 
fileRange <- allFiles[id]
fileList <- paste(dir1, fileRange, sep="/")

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
