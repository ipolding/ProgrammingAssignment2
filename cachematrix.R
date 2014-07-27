## Put comments here that give an overall description of what your
## functions do

## These functions allow for the creation of a cacheMatrix and the fast retrieval of 
## its inverse matrix.

## Write a short comment describing this function

## makeCacheMatrix accepts a matrix as an argument and returns a list representing the matrix 
## with a retrievable inverse value  by providing getter and setter methods for the 
## source matrix and its inverse value

makeCacheMatrix <- function(x = matrix()) {
  inverse = NULL
  
  setMatrix <- function(y) {
    # x belongs to the parent environment
    x <<- y
    # x belongs to the parent environment
    inverse <<-NULL            
  }
  
  getMatrix <- function() {
    x
  }
  
  setInverse <- function(z) {
    inverse <<- z
  }
  
  getInverse <- function() {
    inverse
  }     
  
  list(
    getMatrix = getMatrix, setMatrix = setMatrix, 
    getInverse = getInverse, setInverse = setInverse)    
}


## Write a short comment describing this function

## cacheSolve accepts a cacheMatrix list and returns its inverse. If the cacheMatrix has not
## already got a value for its inverse, cacheMatrix will calculate it. Otherwise, cacheSolve
## retrieves the pre-calculated inverse value from the cacheMatrix

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
      message("Getting cached inverse")
      return(inv)
    }
    data <- x$getMatrix()
    inv <- solve(data)
    x$setInverse(inv)
    inv
  }
