## The following functions calculate and cache the inverse of a matrix
## for square invertible matrices. This is particularly useful for large
## matrices where calculating the inverse can be a costly operation and
## must be performed multiple times.

## makeCacheMatrix creates a special matrix object that can be used to
## store a matrix and its associated inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverseValue) i <<- inverseValue
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve calculates the inverse of a square invertible matrix.
## If the value has been previously calculated, cacheSolve returns
## the cached value of the function. If not, cacheSolve calculates
## the inverse value, stores it for future use, and returns it to the
## caller.

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
