# The following pair of functions provide an API for caching solutions to matrix inverses  
# in order to avoid computing it repeatedly. 
# Example of usage:
#
# source("cachematrix.R")
#
# matrix <- replicate(2000, rnorm(2000)) # Create a large matrix
# m <- makeCacheMatrix(matrix) # Wrap matrix using makeCacheMatrix
# i <- cacheSolve(m) # Solves inverse for the first time
# 
# i <- cacheSolve(m) # subsequent calls will use cache and output "getting cache solution"

makeCacheMatrix <- function(x = matrix()) {
  # A matrix wrapper that can holds it's inverse.
  # This wrapper can be used by cacheSolve function to solve the inverse or get it from the cache.
  #
  # Args:
  #   x: matrix for whom we want to solve it's invese
  #
  # Returns:
  #   A list with setters and getter for either the matrix or it's inverse.
  
  i <- NULL
  set <- function(y) {
      x <<- y
      i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  # Solves the inverse of 'x' by either using the solve functon or a cache.
  # Uses the cached solution if it is available (and outputs "getting cache solution"). 
  # If it's not cached, calculates the inverse and updates the cache.
  # 
  # Args:
  #   x: matrix wrapper created by makeCacheMatrix function
  #
  # Returns:
  #   A matrix that is the inverse of 'x'
  
  i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cache solution")
    return(i)
  }
  matrix <- x$get()
  i <- solve(matrix, ...)
  x$setInverse(i)
  i
}
