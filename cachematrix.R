## Overall description of what my functions do :
## We need to write a function that computes and caches the inverse of a matrix 
## to avoid time consumming when computing the inverse of a matrix.
## To acheive our purpose, we write two function makeCacheMatrix and cacheSolve as described below.


## Short comment describing makeCacheMatrix function  
## makeCacheMatrix creates a special "matrix" containing functions to : 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  setmatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
}


## Short comment describing cacheSolve function

##cacheSolve below computes the inverse of the special "matrix" created with the function makeCacheMatrix. 
## cacheSolve first checks to see if the inverse has already been calcuated. 
## If yes, it gets the inverse from the cache and skips the computation. 
## Otherwise, it computes the inverse of the data and sets the value of 
## the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getmatrix()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
