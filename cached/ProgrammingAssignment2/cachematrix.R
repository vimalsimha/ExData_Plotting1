## Put comments here that give an overall description of what your
## functions do
## Inverts a matrix while checking if it has already been done. If so, it is
## retrieved from the cache as repeatedly inverting a matrix is computationally
## expensive.


## Write a short comment describing this function
## To create a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



## Write a short comment describing this function

## To compute the inverse of a matrix, and to retrieve it from cache if
already calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
