## Put comments here that give an overall description of what your
## functions do

## Create a special matrix that keeps a cache of its inverse

makeCacheMatrix <- function(m = matrix()) {
  
  Inv <- NULL
  
   set <- function(y) {
     m <<- y
     Inv <<- NULL
   }
  get <- function(){
    
    m
  }
  setInv <- function(usrInv) {
    
   Inv <<- usrInv
  }  
  getInv <- function() {
    
    Inv 
  }   
    
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## claculate the inverse of the matrx fromt he above special matrix function if the inverse deosnt exist. 

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'm'
  Inv <- m$getInv()
  
  if(!is.null(Inv)){
    message("retrieving existing inverse value")
    return(Inv)
}

message("calculating new inverse value")



usrMatrix <- m$get()

usrInv <- solve(usrMatrix, ...)

m$setInv(usrInv)

Inv
}