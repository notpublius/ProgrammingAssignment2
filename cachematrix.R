## Together these two functions create a mechanism for creating an inverse-caching
## matrix "object".

## This function "wraps" a matrix as a list of methods that enable the inverse
## to be cached using variables within this function's environment.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {
        x
    }
    setinv <- function(inverse) {
        inv <<- inverse
    }
    getinv <- function() {
        inv
    }
    list(set = set , get = get , setinv = setinv , getinv = getinv)
}


## cacheSolve takes a list, as returned by makeCacheMatrix
## The function will calculate and store (cache) the inverse if it is not present
## Then the inverse will be returned.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if( is.null(i) ) {
      i <- solve(x$get(), ...)
      x$setinv(i)
    }
    i
}
