## The solution implements an approach to speedup such time-consuming operation
## as matrix inverse (using "solve")
## Assumption: input matrix is invertible 

## Function makeCacheMatrix creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
    mi <- NULL
    set <- function(y) {
        x <<- y
        mi <<- NULL
    }
    get <- function() x
    setInverse <- function(inversed) mi <<- inversed
    getInverse <- function() mi
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Function "cacheSolve" computes the inverse of the special "matrix" returned by
## "makeCacheMatrix". If the inverse has already been calculated and the matrix
## has not changed, then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    mi <- x$getInverse()
    if(!is.null(mi)) {
        message("getting cached data")
        return(mi)
    }
    data <- x$get()
    mi <- solve(data, ...)
    x$setInverse(mi)
    mi
}
