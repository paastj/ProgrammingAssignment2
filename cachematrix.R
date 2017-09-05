## This pair of functions is intended to calculate the inverse of a matrix with the ability to cache the calculated result for further usage

## This function creates a caching-aware matrix equipped with helper get/set functions
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function calculates the inverse of a given matrix trying to retrieve the cached result at first
cacheSolve <- function(x, ...) {
    i <- x$getinv()
    
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
