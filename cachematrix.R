## The code explains how caching and lexical scoping work in R.
## Here 2 funcions are used namely makeCacheMatrix, which creates
## a special matrix object, and cacheSolve that fetches the
## inverse of the special matrix from the cache or computes it

## makeCacheMatrix is a function that takes one argument of
## matrix class and creates a special matrix object. Internally
## it has 4 functions to set and get the value as well as the
## inverse of matrix object

makeCacheMatrix <- function(x = matrix()) {

        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve is a function that takes special matrix object as
## one of its arguments and returns its inverse as a matrix.
## Firstly, it checks whether its inverse exists in the cache
## and fetches it if it does, else it computes the inverse of
## the matrix and stores it in the cache for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setinverse(i)
        i
}
