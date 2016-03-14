# Matrix inversion is typically a costly computation, so caching the 
# inverse of a matrix performs better than calculatng the inverse 
# of a matrix repeatedly
#
# Function "makeCacheMatrix" - creates a special matrix object that is 
#     capable of caching its inverse
# Function "cacheSolve" - computes the inverse of a special matrix returned 
#     by makeCacheMatrix
#
# Usage Example:
#     b <- matrix(c(3,2,0,0,0,1,2,-2,1),3,3)
#     cacheSolve(makeCacheMatrix(b))


# "makeCacheMatrix" creates a special matrix object that is 
#     capable of caching its inverse.  It contains the following functions:
#         set         stores a matrix
#         get         returns the stored matrix
#         setinverse  stores the inverse of the matrix in cache
#         getinverse  returns the invers of the matrix from cache

makeCacheMatrix <- function(x = matrix()) {
  
  # Create a variable for caching the inverse.  Initially, the value assigned 
  #  to this variable is NULL.
  inverseCache <- NULL
  
  # stores a matrix
  set <- function(y = matrix()) {
    x <<- y
    inverseCache <<- NULL
  }
  
  # returns the stored matrix
  get <- function() x
  
  # stores the inverse of the matrix in cache
  setinverse <- function(solve) inverseCache <<- solve

  # returns the invers of the matrix from cache
  getinverse <- function() inverseCache
  
  # return a list in which each named element of the list is a function
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# computes the inverse of a special matrix returned by makeCacheMatrix
cacheSolve <- function(x = makeCacheMatrix(matrix(rnorm(16),4,4)), ...) {
  
  # attempt to get the cached inverse of the matrix
  i <- x$getinverse()

  # if the cached inverse is found (not NULL), then return it
  if ( !is.null(i) ) {
    message("Getting cached data")
    return(i)
  } 
  
  # if the cached inverse is not found (is NULL), then calculate
  #  and cache the inverse
  i <- solve(x$get())
  x$setinverse(i)
  
  # return the inverse
  i
}