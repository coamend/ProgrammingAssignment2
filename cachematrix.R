## This function creates a dummy list that is used to cache the matrix inverse.
## The output of this function is used by cacheSolve to either solve the inverse
## or retrieve the previously cached solution to the matrix inverse if it is
## available.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function checks to see if the solution to the matrix inverse is already
## stored in the cache and return it. If it is not in the cache, then the 
## inverse is calculated and stored in the cache.

cacheSolve <- function(x, ...) {
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
