## These set of functions allow the use of matrix objects which can store their
## own inverse. This is used by a specialized cacheSolve function to avoid an expensive
## recalculation.

## Create a matrix object which has function calls to get and set the matrix,
## as well as get and set the inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Returns a matrix that is the inverse of 'x'.
## Requires input of a "cacheMatrix" object which is square.

cacheSolve <- function(cacheMatrix, ...) {
    m <- cacheMatrix$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- cacheMatrix$get()
    m <- solve(data, b = 0, ...)
    cacheMatrix$setInverse(m)
    
    m
}
