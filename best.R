#Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
#outcome name. The function reads the outcome-of-care-measures.csv le and returns a character vector
#with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specied outcome
#in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
#be one of \heart attack", \heart failure", or \pneumonia". Hospitals that do not have data on a particular
#outcome should be excluded from the set of hospitals when deciding the rankings.
#Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
#be sorted in alphabetical order and the rst hospital in that set

best <- function(state, outcome){
  
  dir <- getwd()  
  dir1 <- paste(dir, "rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", sep="/")
  df <- read.csv(dir1, colClasses = "character")
  
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
              
              df.state <- df[, "State"]
              df.state.usrsel <- df[df$State==state,]
              df.state.usrsel.hak <- df.state.usrsel[, 11]
              df.state.usrsel.hak.num <- as.numeric(df.state.usrsel.hak)    
              
              
              df.state.usrsel.hak.num.rloc <- which.min(df.state.usrsel.hak.num)              
              hospNames.state <- df.hospname <- df.state.usrsel[,2]              
              hospName <- hospNames.state[df.state.usrsel.hak.num.rloc]
        
              rval1 <- df.state.usrsel.hak.num[df.state.usrsel.hak.num.rloc]
        
              df.state.usrsel.hak.num2 <- df.state.usrsel.hak.num[-rval1]
        
              df.state.usrsel.hak.num.rloc2 <- which.min(df.state.usrsel.hak.num2)
              rval2 <- df.state.usrsel.hak.num2[df.state.usrsel.hak.num.rloc2]
        
              ##check for tie
             
                      if(rval1==rval2){
        
                        hospName1 <- hospNames.state[df.state.usrsel.hak.num.rloc] 
                        hospName2 <- hospNames.state[df.state.usrsel.hak.num.rloc2]   
        
                        s <- sort(hospName1, hospName2, decreasing=TRUE) 
                        hospName <- s[1]
                        return(hospName)
                      }      
              
              return(hospName)        
              
                
              }    
                if(outcome == "heart failure"){
            
            ##names(df)
            names.df <- names(df)
            df.state <- df[, "State"]
            
            df.state <- df[, "State"]
            df.state.usrsel <- df[df$State==state,]
            df.state.usrsel.hak <- df.state.usrsel[, 17]
            df.state.usrsel.hak.num <- as.numeric(df.state.usrsel.hak)    

            df.state.usrsel.hak.num.rloc <- which.min(df.state.usrsel.hak.num)

            hospNames.state <- df.hospname <- df.state.usrsel[,2]
            
            hospName <- hospNames.state[df.state.usrsel.hak.num.rloc]

            rval1 <- df.state.usrsel.hak.num[df.state.usrsel.hak.num.rloc]
            df.state.usrsel.hak.num2 <- df.state.usrsel.hak.num[-rval1]
            df.state.usrsel.hak.num.rloc2 <- which.min(df.state.usrsel.hak.num2)
            rval2 <- df.state.usrsel.hak.num2[df.state.usrsel.hak.num.rloc2]

            ##check for tie
            
            if(all(rval1==rval2)==TRUE){

              hospName1 <- hospNames.state[df.state.usrsel.hak.num.rloc] 
              hospName2 <- hospNames.state[df.state.usrsel.hak.num.rloc2]   

              s <- sort(hospName1, hospName2, decreasing=TRUE) 
              hospName <- s[1]
              return(hospName)
            }      
            
            return(hospName)        
            
            
          }   
                    if(outcome == "pneumonia"){
                            
                            ##names(df)
                            names.df <- names(df)
                            df.state <- df[, "State"]
                            
                            df.state <- df[, "State"]
                            df.state.usrsel <- df[df$State==state,]
                            df.state.usrsel.hak <- df.state.usrsel[, 23]
                            df.state.usrsel.hak.num <- as.numeric(df.state.usrsel.hak)    
                            
                            
                            df.state.usrsel.hak.num.rloc <- which.min(df.state.usrsel.hak.num)
                            
                            hospNames.state <- df.hospname <- df.state.usrsel[,2]
                            
                            hospName <- hospNames.state[df.state.usrsel.hak.num.rloc]

                            rval1 <- df.state.usrsel.hak.num[df.state.usrsel.hak.num.rloc]

                            df.state.usrsel.hak.num2 <- df.state.usrsel.hak.num[-rval1]

                            df.state.usrsel.hak.num.rloc2 <- which.min(df.state.usrsel.hak.num2)
                            rval2 <- df.state.usrsel.hak.num2[df.state.usrsel.hak.num.rloc2]

                            ##check for tie
                            
                            if(rval1==rval2){

                              hospName1 <- hospNames.state[df.state.usrsel.hak.num.rloc] 
                              hospName2 <- hospNames.state[df.state.usrsel.hak.num.rloc2]   

                              s <- sort(hospName1, hospName2, decreasing=TRUE) 
                              hospName <- s[1]
                              return(hospName)
                            }      
                            
                            return(hospName)        
                            
                            
                          }
}