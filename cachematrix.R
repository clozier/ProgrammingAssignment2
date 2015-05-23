## The following two functions cache the inverse of a matrix and might be used
## within a larger script to avoid having to repeatedly perform the calculation.
## I've aggressively added comments throughout for my own learning.

## This (first) function creates a special matrix object. Assign this function 
## to object named "makeCacheMatrix". Sets default value of argument x to the 
## function "matrix".

makeCacheMatrix <- function(x = matrix()) { 
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}

## This (second) function computes the invers of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then the cacheSolve retrieves teh inverse from 
## the cache.

## Assigns to object named "cacheSolve" this function of x and additional (...) 
## arguments. Sets m = "getmatrix" from x. If m is NOT NULL, function returns m.
## If m IS NULL, goes back to run other function to "get" matrix and then runs
## "solve" on m to get inverse.

cacheSolve <- function(x, ...) {
        m <- x$getmatrix() 
        if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        } 
        data <- x$get() 
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
