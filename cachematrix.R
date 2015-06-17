## This program contains two functions:
## 1. To convert a matrix into a cacheable matrix to store the input and also a calculated matrix.
## 2. To output the inverse of a matrix and cache the result for subsequent calls.
##
##  USAGE: 
##    inpMatrix <- matrix(1:4, c(2,2))
##    splMatrix <- makeCacheMatrix( inpMatrix )
##    cacheSolve(splMatrix)
##    >> run the cacheSolve again to see the data retrieved from cache
##    cacheSolve(splMatrix)
##   
##    inpMatrix <- matrix(5:8, c(2,2))
##    splMatrix <- makeCacheMatrix( inpMatrix )
##    cacheSolve(splMatrix)   >> this will recalculate the inv matrix
##    cacheSolve(splMatrix)   >> this will use the cached value.
##


## Convert an input matrix into an object that stores the matrix and caches the inverse of the matrix as a "cached Matrix"
##
## input
## @param x - input matrix
##
## returns
##   a vector that exposes functions to set and get the input matrix and cached inverse of the matrix
##
makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  setinvm <- function(invmatrix) invm <<- invmatrix
  getinvm <- function() invm
  list(set = set, get = get,
       setinvm = setinvm,
       getinvm = getinvm)
}


## output the inverse of a matrix and cache the result for subsequent calls.
##
## input
## @param x - takes the special object that contains the input matrix and cached inverse of matrix
##
## returns
##   a matrix that contains the inverse of the input matrix (it is calculated only if the cache is empty).
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invm <- x$getinvm()
  if(!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  data <- x$get()
  invm <- solve(data, ...)
  x$setinvm(invm)
  invm
}
