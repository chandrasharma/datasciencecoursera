## Write the following functions:
##1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
##above. If the inverse has already been calculated (and the matrix has not changed), then the
##cachesolve should retrieve the inverse from the cache.
##Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a
##square invertible matrix, then solve(X) returns its inverse.
##For this assignment, assume that the matrix supplied is always invertible.

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