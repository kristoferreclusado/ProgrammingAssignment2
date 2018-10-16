## The goal of the assignment is to cache the inverse of a matrix using two functions: one to create
## the matrix and one to perform the computation.

## makeCacheMatrix is a function that creates the matrix so that the inverse of the input can be cached.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- solve(x)
        getinverse <- function() inv
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve is a function that computes the inverse of the created matrix.
## if the inverse is already calculated and does not change, then the function retrieves the cached data.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
