## The following functions will cache the inverse matrix as this
## computation is very expensive.
##
## Example:
##   c <- rbind(c(1, -1/4), c(-1/4, 1))
##   cm <- makeCacheMatrix(c)
##   cacheSolve(cm)
##   cacheSolve(cm)
##
## Note: This work is based completely on the example shown for this assignment.


## This function will take the matrix specified by the user and
## add the following functions to it:
## set - initializes the cache
## get - returns the matrix from the user
## setinverse - saves the matrix to the cache
## getinverse - returns the matrix stored in the cache

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function will solve the inverse matrix from a matrix built with
## the makeCacheMatrix function.
## This function uses the R solve function and it will pass the additional
## arguments to it.
## The argument X needs to be the result from the makeCacheMatrix function.
## This function will check first if we already cached the results and return it
## and display in the screen that the values are from the cache.
## In case that it is not solved yet, it will solve it and store it to the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
