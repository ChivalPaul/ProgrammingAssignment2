## These functions can cache and compute the inverse of a matrix.

## This function creates a special "matrix" object that can cache 
## the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        setM <- function(Value) {
                x <<- Value
                cache <<- NULL
        }
        getM <- function() {
                x
        }
        cacheInv <- function(solve) {
                cache <<- solve
        }
        getInv <- function() {
                cache
        }
        list(setM = setM, getM = getM, cacheInv = cacheInv, getInv = getInv)
}

## This function can compute the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated and the matrix has not changed, then the 
## cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$getInv()
        if(!is.null(inverse)) {
                message("Getting cached data")
                return(inverse)
        }
        data <- x$getM()
        inverse <- solve(data)
        x$cacheInv(inverse)
        inverse
}
