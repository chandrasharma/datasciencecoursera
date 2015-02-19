#Write a function called rankall that takes two arguments: an outcome name (outcome) and a hospital rank-
#ing (num). The function reads the outcome-of-care-measures.csv le and returns a 2-column data frame
#containing the hospital in each state that has the ranking specied in num. For example the function call
#rankall("heart attack", "best") would return a data frame containing the names of the hospitals that
#are the best in their respective states for 30-day heart attack death rates. The function should return a value
#for every state (some may be NA). The rst column in the data frame is named hospital, which contains
#the hospital name, and the second column is named state, which contains the 2-character abbreviation for
#the state name. Hospitals that do not have data on a particular outcome should be excluded from the set of
#hospitals when deciding the rankings.
#Handling ties. The rankall function should handle ties in the 30-day mortality rates in the same way
#that the rankhospital function handles ties.

rankall <- function(outcome, num){
  
  dir <- getwd()  
  dir1 <- paste(dir, "outcome-of-care-measures.csv", sep="/")
  df <- read.csv(dir1, colClasses = "character")
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
      #check for state validity
      v1 <- c("heart attack", "heart failure", "pneumonia")
      v2 <- c(outcome)

          if(v2 %in% v1 == FALSE){
            stop("Invalid outcome")  
          }     
  
#sub setting based on state

hospName<-NULL
hospNamecur <- character()
outpt<-NULL

      if(outcome == "heart attack"){

              hospNameout<-NULL
              hospNamein<-NULL
              df.state.usrsel.lst<-NULL

                            
              names.df <- names(df)
              df.state <- df[, "State"]
              df.state.uniq<- unique(df.state)
              df.state.uniq.len<- length(df.state.uniq)
              #print(df.state.uniq)
                    for(i in 1:df.state.uniq.len){
                        
                        df.state.usrsel <- df[df$State==df.state.uniq[i],] 
                        
                        #print(df.state.usrsel)
                        
                        df.state.usrsel.lst<-append(df.state.usrsel.lst,df.state.uniq[i])
                        #print(df.state.usrsel.lst)
                        df.state.usrsel.haf <- df.state.usrsel[, 11]
                        df.state.usrsel.haf.num <- as.numeric(df.state.usrsel.haf)
                        hospNames.state <- df.hospname <- df.state.usrsel[,2]
                        hospCount<-length(hospNames.state)
                         #print(hospCount)
                        #print(df.state.uniq[i])
                        #print("out")
                        hospName<-append(hospName, hospNamecur)
                        if(num == "best"){      
                          
                          #print("1")
                          
                          hospNamecur <- hospNames.state[(which.min(df.state.usrsel.haf.num))]                    
                          hospName <-append(hospName,hospNamecur)
                          
                        }else if(num == "worst"){ 
                          
                         # print("2")
                          
                          hospNamecur <- hospNames.state[(which.max(df.state.usrsel.haf.num))]                            
                          hospName <-append(hospName,hospNamecur)
                        }else if(hospCount < num){
                         
                          print("Please enter a number less than 200 for num and try your luck")
                          break(NA) 
                          
                          }else if(num < hospCount){
                            
                            #hospName <- NULL 
                            
                            #print("2")
                            #print(df.state.uniq[i])
                              
                              df.state.usrsel.haf.num.ordr <- order(df.state.usrsel.haf.num)
                              rank1 <- df.state.usrsel.haf.num.ordr[num]
                              rankOrigRowVal <- df.state.usrsel.haf.num[rank1]
                              
                              #print(rankOrigRowVal)
                              
                              df.state.usrsel.haf.num2 <- df.state.usrsel.haf.num[-rankOrigRowVal]
                              df.state.usrsel.haf.num2.ordr <- order(df.state.usrsel.haf.num2)                                              
                              rank1Dup <- df.state.usrsel.haf.num2.ordr[num]                                              
                              rankOrigRowVal2 <- df.state.usrsel.haf.num2[rank1Dup]
                              
                              #print(rankOrigRowVal2)
                              
                              hospNamecur <-hospNames.state[rank1]                            
                              #hospName <-append(hospName,hospNamecur)
                             #print(hospNamecur)
                              
                            
                              if(is.na(rankOrigRowVal|rankOrigRowVal2)){
                                
                               
                                #print("3")
                                
                                hospNamecur<-NA                                                    
                                #hospName<-append(hospName,hospNamecur)                    
                              }else if (all(rankOrigRowVal==rankOrigRowVal2, na.rm=TRUE)){
                                
                                #print("4")

                                hospName1 <- hospNames.state[rank1]
                                hospName2 <- hospNames.state[rank1Dup]
                                hospNamecur <- sort(hospName1, hospName2, decreasing=TRUE)
                                x<-append(hospName,hospNamecur)
                                # hospNamein <- append(hospNamein, hospNamecur) 
                                lenn<-length(hospNamecur)
                                #print(hospNamecur)
                              }                        
                           
                            }
                        
                        #print(hospName)
                        #return(hospName)
                        #outpt<-data.frame("hospital"=hospName,"state"=df.state.usrsel.lst)
                        outpt1<-cbind("hospital"=hospName,"state"=df.state.usrsel.lst)
                        outpt<-data.frame(outpt1)
                          }
              
             
                  
                  }

          if(outcome == "heart failure"){
            
            hospNameout<-NULL
            hospNamein<-NULL
            df.state.usrsel.lst<-NULL
            
            
            names.df <- names(df)
            df.state <- df[, "State"]
            df.state.uniq<- unique(df.state)
            df.state.uniq.len<- length(df.state.uniq)
            #print(df.state.uniq)
            for(i in 1:df.state.uniq.len){
              
              df.state.usrsel <- df[df$State==df.state.uniq[i],] 
              
              #print(df.state.usrsel)
              
              df.state.usrsel.lst<-append(df.state.usrsel.lst,df.state.uniq[i])
              #print(df.state.usrsel.lst)
              df.state.usrsel.haf <- df.state.usrsel[, 17]
              df.state.usrsel.haf.num <- as.numeric(df.state.usrsel.haf)
              hospNames.state <- df.hospname <- df.state.usrsel[,2]
              hospCount<-length(hospNames.state)
              #print(hospCount)
              #print(df.state.uniq[i])
              #print("out")
              hospName<-append(hospName, hospNamecur)
              if(num == "best"){      
                
                #print("1")
                
                hospNamecur <- hospNames.state[(which.min(df.state.usrsel.haf.num))]                    
                hospName <-append(hospName,hospNamecur)
                
              }else if(num == "worst"){ 
                
                # print("2")
                
                hospNamecur <- hospNames.state[(which.max(df.state.usrsel.haf.num))]                            
                hospName <-append(hospName,hospNamecur)
              }else if(hospCount < num){
                
                print("Please enter a number less than 200 for num and try your luck")
                break(NA)  
                
              }else if(num < hospCount){
                
                #hospName <- NULL 
                
                #print("2")
                #print(df.state.uniq[i])
                
                df.state.usrsel.haf.num.ordr <- order(df.state.usrsel.haf.num)
                rank1 <- df.state.usrsel.haf.num.ordr[num]
                rankOrigRowVal <- df.state.usrsel.haf.num[rank1]
                
                #print(rankOrigRowVal)
                
                df.state.usrsel.haf.num2 <- df.state.usrsel.haf.num[-rankOrigRowVal]
                df.state.usrsel.haf.num2.ordr <- order(df.state.usrsel.haf.num2)                                              
                rank1Dup <- df.state.usrsel.haf.num2.ordr[num]                                              
                rankOrigRowVal2 <- df.state.usrsel.haf.num2[rank1Dup]
                
                #print(rankOrigRowVal2)
                
                hospNamecur <-hospNames.state[rank1]                            
                #hospName <-append(hospName,hospNamecur)
                #print(hospNamecur)
                
                
                if(is.na(rankOrigRowVal|rankOrigRowVal2)){
                  
                  
                  #print("3")
                  
                  hospNamecur<-NA                                                    
                  #hospName<-append(hospName,hospNamecur)                    
                }else if (all(rankOrigRowVal==rankOrigRowVal2, na.rm=TRUE)){
                  
                  #print("4")
                  
                  hospName1 <- hospNames.state[rank1]
                  hospName2 <- hospNames.state[rank1Dup]
                  hospNamecur <- sort(hospName1, hospName2, decreasing=TRUE)
                  x<-append(hospName,hospNamecur)
                  # hospNamein <- append(hospNamein, hospNamecur) 
                  lenn<-length(hospNamecur)
                  #print(hospNamecur)
                }                        
                
              }
              
              #print(hospName)
              #return(hospName)
              #outpt<-data.frame("hospital"=hospName,"state"=df.state.usrsel.lst)
              outpt1<-cbind("hospital"=hospName,"state"=df.state.usrsel.lst)
              outpt<-data.frame(outpt1)
            }
            
            
            
          }

                if(outcome == "pneumonia"){
  
  hospNameout<-NULL
  hospNamein<-NULL
  df.state.usrsel.lst<-NULL
  
  
  names.df <- names(df)
  df.state <- df[, "State"]
  df.state.uniq<- unique(df.state)
  df.state.uniq.len<- length(df.state.uniq)
  #print(df.state.uniq)
  for(i in 1:df.state.uniq.len){
    
    df.state.usrsel <- df[df$State==df.state.uniq[i],] 
    
    #print(df.state.usrsel)
    
    df.state.usrsel.lst<-append(df.state.usrsel.lst,df.state.uniq[i])
    #print(df.state.usrsel.lst)
    df.state.usrsel.haf <- df.state.usrsel[, 17]
    df.state.usrsel.haf.num <- as.numeric(df.state.usrsel.haf)
    hospNames.state <- df.hospname <- df.state.usrsel[,2]
    hospCount<-length(hospNames.state)
    #print(hospCount)
    #print(df.state.uniq[i])
    #print("out")
    hospName<-append(hospName, hospNamecur)
    if(num == "best"){      
      
      #print("1")
      
      hospNamecur <- hospNames.state[(which.min(df.state.usrsel.haf.num))]                    
      hospName <-append(hospName,hospNamecur)
      
    }else if(num == "worst"){ 
      
      # print("2")
      
      hospNamecur <- hospNames.state[(which.max(df.state.usrsel.haf.num))]                            
      hospName <-append(hospName,hospNamecur)
    }else if(hospCount < num){
      
      
      #print("1")
      print("Please enter a number less than 200 for num and try your luck")
      break(NA) 
      
      
    }else if(num < hospCount){
      
      #hospName <- NULL 
      
      #print("2")
      #print(df.state.uniq[i])
      
      df.state.usrsel.haf.num.ordr <- order(df.state.usrsel.haf.num)
      rank1 <- df.state.usrsel.haf.num.ordr[num]
      rankOrigRowVal <- df.state.usrsel.haf.num[rank1]
      
      #print(rankOrigRowVal)
      
      df.state.usrsel.haf.num2 <- df.state.usrsel.haf.num[-rankOrigRowVal]
      df.state.usrsel.haf.num2.ordr <- order(df.state.usrsel.haf.num2)                                              
      rank1Dup <- df.state.usrsel.haf.num2.ordr[num]                                              
      rankOrigRowVal2 <- df.state.usrsel.haf.num2[rank1Dup]
      
      #print(rankOrigRowVal2)
      
      hospNamecur <-hospNames.state[rank1]                            
      #hospName <-append(hospName,hospNamecur)
      #print(hospNamecur)
      
      
      if(is.na(rankOrigRowVal|rankOrigRowVal2)){
        
        
        #print("3")
        
        hospNamecur<-NA                                                    
        #hospName<-append(hospName,hospNamecur)                    
      }else if (all(rankOrigRowVal==rankOrigRowVal2, na.rm=TRUE)){
        
        #print("4")
        
        hospName1 <- hospNames.state[rank1]
        hospName2 <- hospNames.state[rank1Dup]
        hospNamecur <- sort(hospName1, hospName2, decreasing=TRUE)
        x<-append(hospName,hospNamecur)
        # hospNamein <- append(hospNamein, hospNamecur) 
        lenn<-length(hospNamecur)
        #print(hospNamecur)
      }                        
      
    }
    
    #print(hospName)
    #return(hospName)
    #outpt<-data.frame("hospital"=hospName,"state"=df.state.usrsel.lst)
    outpt1<-cbind("hospital"=hospName,"state"=df.state.usrsel.lst)
    outpt<-data.frame(outpt1)
  }
  
  
  
}  
return(outpt)
}
