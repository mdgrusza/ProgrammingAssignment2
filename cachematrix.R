## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 dm <-dim(x)
 
 #checks if the matrix is inversible 
 if(dm[1]==dm[2] && det(x)!=0){ #For example, if X is a square invertible matrix, then solve(X) returns its inverse.
  s <- NULL  
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
 }
 else{ message("matrix is not invertible")} # returns an error
  
}


## Write a short comment describing this function
#This function computes the inverse of matrix
cacheSolve <- function(x, ...) {
    
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...) #
  x$setsolve(s)
  s
  
}
