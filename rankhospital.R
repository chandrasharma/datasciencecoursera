rankhospital <- function(state, outcome, num){
  


  dir <- getwd()  
  dir1 <- paste(dir, "outcome-of-care-measures.csv", sep="/")
  df <- read.csv(dir1, colClasses = "character")
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #check for state validity
  
      stateCol1 <- unique(df[,7])  
      stateCol <- c(stateCol1)  
      Count <- length(stateCol)
 
            v1 <- c("heart attack", "heart failure", "pneumonia")
            v2 <- c(outcome)
            v3 <- c(state)


      if(v3 %in% stateCol == FALSE ){
  stop("Invalid state") 
  
}         

          if(v2 %in% v1 == FALSE){
  stop("Invalid outcome")  
}      

#sub setting based on state

                if(outcome == "heart attack"){
      
      ##names(df)
                  names.df <- names(df)
                  df.state <- df[, "State"]
            
                  df.state.usrsel <- df[df$State==state,]
                  df.state.usrsel.hak <- df.state.usrsel[, 11]
                  df.state.usrsel.hak.num <- as.numeric(df.state.usrsel.hak)    
                  hospNames.state <- df.hospname <- df.state.usrsel[,2]
                
                      if(num == "best"){
        
        hospName <- hospNames.state[(which.min(df.state.usrsel.hak.num))]
        
        return(hospName)
        
      }
                          if(num == "worst"){
        
        hospName <- hospNames.state[(which.max(df.state.usrsel.hak.num))]
        return(hospName)
      }
      
                              if(num > 0){
                                num<-num-1

                                df.state.usrsel.hak.num.ordr <- order(df.state.usrsel.hak.num)
                                rank1 <- df.state.usrsel.hak.num.ordr[num]
                                rankOrigRowVal <- df.state.usrsel.hak.num[rank1]
                                
                                df.state.usrsel.hak.num2 <- (df.state.usrsel.hak.num[-rankOrigRowVal])
                                df.state.usrsel.hak.num2.ordr <- order(df.state.usrsel.hak.num2)
                                #print(df.state.usrsel.hak.num2)
                                
                                rank1Dup <- df.state.usrsel.hak.num2.ordr[num]
                                rankOrigRowVal2 <- df.state.usrsel.hak.num2[rank1Dup]
   
                                    if ((all(rankOrigRowVal==rankOrigRowVal2))==TRUE){
                             
                                      hospName <- hospNames.state[rank1]
                                      hospName2 <- hospNames.state[rank1Dup]
                                      
                                      hospName <- sort(hospName, hospName2, decreasing=TRUE)
                
                return(hospName)
              }
        
      }

      ##check for tie
      
      hospName <- hospNames.state[rank1]
        
        return(hospName)        
        
        
      } 
                      if(outcome == "heart failure"){

              names.df <- names(df)
              df.state <- df[, "State"]
              
              df.state <- df[, "State"]
              df.state.usrsel <- df[df$State==state,]
              df.state.usrsel.haf <- df.state.usrsel[, 17]
              df.state.usrsel.haf.num <- as.numeric(df.state.usrsel.haf)
              hospNames.state <- df.hospname <- df.state.usrsel[,2]
 
              if(num == "best"){      
                
                hospName <- hospNames.state[(which.min(df.state.usrsel.haf.num))]                
                return(hospName)
                
              }
              if(num == "worst"){
                
                hospName <- hospNames.state[(which.max(df.state.usrsel.haf.num))]
                return(hospName)
              }
              
              if(num != 0){

                df.state.usrsel.haf.num.ordr <- order(df.state.usrsel.haf.num)
                rank1 <- df.state.usrsel.haf.num.ordr[num]
                rankOrigRowVal <- df.state.usrsel.haf.num[rank1]
                df.state.usrsel.haf.num2 <- df.state.usrsel.haf.num[-rankOrigRowVal]
                df.state.usrsel.haf.num2.ordr <- order(df.state.usrsel.haf.num2)
    
                rank1Dup <- df.state.usrsel.haf.num2.ordr[num]

                rankOrigRowVal2 <- df.state.usrsel.haf.num2[rank1Dup]

                if(is.na(rankOrigRowVal2) | is.na(rankOrigRowVal) == TRUE){
                  
                  stop("cannot rank")
                  
                }
                hospName <- hospNames.state[rank1]
                
                if (rankOrigRowVal==rankOrigRowVal2){
                  #print("I am here")
                  hospName <- hospNames.state[rank1]
                  hospName2 <- hospNames.state[rank1Dup]
                  hospName <- sort(hospName, hospName2, decreasing=TRUE)
                  
                  return(hospName)
                }
                
              }

              ##check for tie  
              
              return(hospName)              
              
            }

                            if(outcome == "pneumonia"){
          
                              names.df <- names(df)
                              df.state <- df[, "State"]
                              
                              df.state <- df[, "State"]
                              df.state.usrsel <- df[df$State==state,]
                              df.state.usrsel.haf <- df.state.usrsel[, 23]
                              df.state.usrsel.haf.num <- as.numeric(df.state.usrsel.haf)    
          
                              hospNames.state <- df.hospname <- df.state.usrsel[,2]

                    if(num == "best"){
                      
                      hospName <- hospNames.state[(which.min(df.state.usrsel.haf.num))]
                      
                      return(hospName)
                      
                    }
                    if(num == "worst"){
                      
                     
                      hospName <- hospNames.state[(which.max(df.state.usrsel.haf.num))]
                      return(hospName)
                    }
                    
                    if(num > 0){

                      df.state.usrsel.haf.num.ordr <- order(df.state.usrsel.haf.num)
                      rank1 <- df.state.usrsel.haf.num.ordr[num]
                      rankOrigRowVal <- df.state.usrsel.haf.num[rank1]
                      df.state.usrsel.haf.num2 <- df.state.usrsel.haf.num[-rankOrigRowVal]
                      df.state.usrsel.haf.num2.ordr <- order(df.state.usrsel.haf.num2)
                      rank1Dup <- df.state.usrsel.haf.num2.ordr[num]

                      rankOrigRowVal2 <- df.state.usrsel.haf.num2[rank1Dup]

                      if (rankOrigRowVal==rankOrigRowVal2){
                        
                        hospName <- hospNames.state[rank1]
                        hospName2 <- hospNames.state[rank1Dup]
                        
                        hospName <- sort(hospName, hospName2, decreasing=TRUE)
                        
                        return(hospName)
                      }
                      
                    }

                    ##check for tie                
                    return(hospName)                 
                    
                  }
          
}