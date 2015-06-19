# R Programming - Programming Assignment 2

## matrix --> list
## return a list representing a special matrix object with
## get, set, getInverse, and setInverse methods
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## list (representing a special matrix obj) --> matrix
## return the inverse of a matrix
## if the inverse has already been calculated, retrieve it from cache
## else calculate and return the inverse
## ASSUME: input matrix is always invertible
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
