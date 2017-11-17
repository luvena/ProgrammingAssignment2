## Put comments here that give an overall description of what your
## functions do

# These two functions work together to cache and store matrix inverse calculations. 
# The first function creates a "matrix" list to get/store the original matrix and its inverse
# The second function retrieves information from the "matrix" object created by the first
# Assumptions: Inverse can be calculated for the inputed matrix. 

## Write a short comment describing this function
# This function creates a list object that allows setting and retrieving of the stored matrix and its inverse
# 
# Args: 
#  x: a matrix whose inverse will be calculated
# Returns: 
#  list of functions including getting/setting matrix and getting/setting the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  
  #Set the inverse to null initially 
  m <- NULL
  
  #Definition of functions
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  
  #output a list of functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
# This function calculates the inverse of the "matrix object" created above either by
#  1. retrieving from the cache
#  2. computing the inverse and then storing this inverse in the cached object
# 
# Args: 
#  x: matrix object generated from the makeCacheMatrix function
#
# Returns: 
#  Inverse of matrix (and a note if the inverse is retrieve from the cache)

cacheSolve <- function(x, ...) {
  
  # Retrieves inverse matrix from "matrix object"
  m <- x$getinv()
  
  # returns cached data if inverse matrix is available
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # Otherwise, new data is stored and solved
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

