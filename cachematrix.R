## Programming Assignment 2
## R Programming Coursera course
## Michael Ching
## 
## This file has the scripts for two functions. The first caches a matrix's
## inverse and the second can retrieve it or calculate it if it has not been
## cached

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
    # initialize local matrix inverse variable as NULL
    m <- NULL
  
    # this function will take the input of a matrix and set it to a "cached" 
    # matrix in a different environment. It also initializes a "cached" inverse as
    # NULL.
    set <- function(y = matrix()) {
        x <<- y
        m <<- NULL
    }
  
    # this function will retrieve the "cached" matrix from the other environment
    get <- function() x
    
    # this function sets its input to be the "cached" inverse matrix solution in
    # the other environment
    setinverse <- function(inverse) m <<- inverse
    
    # this function retrieves the "cached" inverse matrix solution from the other
    # environment
    getinverse <- function() m
    
    # this combines the 4 functions into a list for easy access
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve should retrieve the inverse from the
## cache.

cacheSolve <- function(x, ...) {
  # retrieve the cached inverse matrix
  local.inverse <- x$getinverse()
  
  # if there is a non-null matrix retrieved, stop and return this cached inverse
  # matrix
  if(!is.null(local.inverse)) {
    message("getting cached data")
    return(local.inverse)
  }
  
  # otherwise retrieve the cached original matrix
  data <- x$get()
  
  # calculate its inverse
  local.inverse <- solve(data, ...)
  
  # store the result in the cached inverse location
  x$setinverse(local.inverse)
  
  # return the solution
  local.inverse
}