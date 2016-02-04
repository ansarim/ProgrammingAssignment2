# This set of 2 functions creates and caches inverse of a inversiable
# matrix.

# makeCacheMatrix() caches the inverse in the parent environment
# User can also clear the cache, get the matrix or set the matrix

makeCacheMatrix <- function(y = matrix()) {
    mi <- NULL
    set <- function(x) {
        y <<- x
        mi <<- NULL
    }
    get <- function() y
    setinverse <- function(inverse) mi <<- inverse
    getinverse <- function() mi
    list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse)
}

# cacheSolve() takes the list object that contains methods to
# set/get/setinverse/getinverse.

# if the inverse matrix is already cached, it will get it from 
# the cache. If not, it will calculate the inverse and stores
# in the variable mi.
# Variable mi is in the environment of where cacheSolve()
# is being called but, is set in the environemt of makeCacheMatrix()
# using <<- operation

cacheSolve <- function(x, ...) {
    mi <- x$getinverse()
    if(!is.null(mi)) {
        message("getting cached inverse matrix")
        return(mi)
    }
    data <- x$get()
    mi <- solve(data, ...)
    x$setinverse(mi)
    mi
    
}