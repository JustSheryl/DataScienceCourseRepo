## This file satisfies Programming Assignment #2  
## It contains 2 functions: makeCacheMatrix() and
## cacheSolve

## makeCacheMatrix creates a matrix that
## can cache its inverse.  NOTE:  only
## square matrices can be inverted.  There
## is no error checking to make sure the
## matrix passed in is square

makeCacheMatrix <- function(x = matrix()) {
    inv<- NULL
    set <- function(y){
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
      
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}


## cacheSolve() returns a matrix that is the inverse of
## the matrix created by makeCacheMatrix.
## When invoked, it will first check the cache to see if
## an inverse exists and will return the cached inverse. 
## Else it will create, cache and return the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    inv <- x$getinv()
    if (!is.null(inv)){
      message ("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
