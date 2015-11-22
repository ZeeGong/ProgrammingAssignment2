## Matrix inversion is a costly computation. It is beneficial to cache the inversion
## of a matrix rather than compute it.  
## The two functions below are used to creat a special object that stores a matrix
## and caches its inversion

## This function can cache the inversion of a matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x<<- y
                inv <<- NULL
        }
        get <- function()x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inversion of the matrix created by makeCacheMatrix.
## If the inversion has been calculated, then the function retrieve the inversion 
## from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
