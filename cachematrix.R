## R program that cache the inverse of a matrix

## makeCacheMatrix will create a special matrix object than can
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve computes the inverse of the special matrix return by 
## makeCacheMatrix

cacheSolve <- function(x, ...) {
         m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
