## The 1st function -  makeCacheMatrix, creates a special "matrix" object that can cache its inverse.
## This type of function is required in the case where caching is beneficial rather than computing repeatedly.
## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly 

## The 1st function - makeCacheMatrix, creates a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

## For this assignment, it is assumed that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
  ## Assign NULL to the inverse of Matrix
  invMat <- NULL
  
  ## Set the value of the matrix
  setMat <- function(y) {
    
    ## Use <<- operator to assign a value to an object in an environment that is different from the current environment.
    x <<- y
    invMat <<- NULL
    
  }
  ## Get the value of the matrix
  getMat <- function() x
  
  ## Set the value of inverse of the matrix
  setInverse <- function(inverse) invMat <<- inverse
  
  ## Get the value of inverse of the matrix
  getInverse <- function() invMat
  
  ## Create a list of function.
  list(set=setMat, get=getMat, cacheInv=setInverse, getInverse=getInverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## This 2nd function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  ## Assign the inverse of the matrix created with the above function.
  invMat <- x$getInverse()
  
  ## first check to see if the inverse has already been calculated.
  ## If so, it gets the inverse from the cache and skips the computation. 
  
  if (!is.null(invMat)) {
    ## If inverse is calculated print the message "Getting Cached Data." and return a matrix that is the inverse of 'x'
    message("Getting Cached Data.")
    return(invMat)
  }
  
  matData <- x$get()
  
  ## Otherwise, Calculate the inverse of the matrix data and set the value of the inverse in the cache via the setInverse function.
  ## Use a solve function to compute the inverse of a square matrix.
  invMat <- solve(matData, ...)
  x$cacheInv(invMat)
  
  ## Now Return a matrix that is the inverse of 'x'
  invMat
  
}


