## makeCacheMatrix and cacheSolve are two functions that can be used
## in conjunction to set a matrix, get the inverse of that matrix,
## and cache that result (the inverse). This way, it won't be necessary
## to recalculate this inverse repeatedly later on. We can simply grab
## our cached result, which will save time and computer resources.


## makeCacheMatrix is used to set the initial matrix.
## This matrix will be grabbed by cacheSolve so cacheSolve can
## perform the inverse operation on it.
## Afterwards, the cacheSolve function will store this result 
## back into the variable that called the makeCacheMatrix function,
## and we can grab that cache'd result from that variable.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y=matrix()) {
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


## cacheSolve checks if there's a prior cache'd matrix.
## If so, it will simply return the cache'd matrix, with
## a message noting that it is cache'd data.
## If not, it will grab the matrix set in makeCacheMatrix
## take the inverse of that matrix, and store it in the 
## variable that called the cacheSolve function.


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
