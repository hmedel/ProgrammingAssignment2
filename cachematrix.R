## This code stores a given matrix, and its inverse, in the cache in
##   order to using it frequently.

## The following function stores a given matrix in the cache and its
   inverse if it is already calculated.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
        set <- function(y) {
                x   <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The following function finds the inverse of the given matrix, if it
##   has not been calculated before.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached matrix")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinv(inv)
        inv
}
