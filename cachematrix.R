## Provides a matrix wrapper that calculates and caches the inverse of the
## matrix.
## Josh Geenen - 2014

## Wrapper for a matrix that allows caching the inverse.
## Functions:
## set: Sets the matrix, resets the cached inverse.
## get: Returns the matrix.
## setinverse: Sets the inverse of the matrix.
## getinverse: Returns the cached inverse of the matrix.  May be NULL.

makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL

  # Sets the matrix, resets the cached inverse.
  set <- function(y) {
    x       <<- y
    inverse <<- NULL
  }

  # Returns the matrix.
  get <- function() x

  # Sets the inverse of the matrix.
  setinverse <- function(inv) inverse <<- inv

  # Returns the cached inverse of the matrix.  May be NULL.
  getinverse <- function() inverse

  list (set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Returns the inverse of the given cacheMatrix x.
## The result of the inverse calculate is cached so repeated calls do
## not calculate the inverse multiple times.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
