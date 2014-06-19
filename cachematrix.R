## The makeCacheMatrix function provides a way to create a matrix that will retain its inverse
## after it calculates the first time using cacheSolve.

## This function creates a matrix that has the property of caching its inverse.
## The inversion of the matrix is cached once calculated for the first time.
## If the matrix is changed using the set function, it will clear the cache, forcing a
## recalculation on the next call of cacheSolve.

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
    
    set <- function(y) {
        message("Setting the matrix and initializing the inverse to null to force it to recalculate ...")
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inverse) i <<- inverse
    
    getInverse <- function() i
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function performs a matrix inversion on matrix x.
## If x has already been inverted, the prior cached inversion result will be returned
## rather than reperforming the inversion.  If it has not already been inverted,
## the inversion is performed and the result cached.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    i <- x$getInverse()
    if(!is.null(i)) {
        message("Returning the cached inverted matrix ...")
        return(i)
    }
    else {
        message("Calculating the inverse and caching ...")
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
    }
}
