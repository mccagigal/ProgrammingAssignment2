## This file contains two functions for caching the inverse of a matrix.
## This procedure is based on the use of the scoping rules of the R language.
## The matrix and its inverse will be stored on the R environment and if we 
## try to compute the inverse of the same matrix we will obtain the data on cache.

## "makeCacheMatrix" create an object that contains the funtions to operate with 
## the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # Initialize the inverse to NULL
    inv <- NULL
    # Create "set()" function which stores a new matrix in the environment.
    # Notice that the inverse is rebooted to NULL.
    set <- function(y) {
        x   <<- y
        inv <<- NULL
    }
    # Create "get()" which returns the stored matrix
    get <- function() x
    # Create "setinverse()" which stores a precomputed inverse matrix
    setinverse <- function(inverse) inv <<- inverse
    # Create "getinverse()" which returns the stored inverse matrix
    getinverse <- function() inv
    # Return a list which contains the aforementioned functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## "cacheSolve" return the inverse of the matrix stored in the environment.
## This functions requires an object from "makeCacheMatrix" function to 
## access to the target matrix.
## The "makeCacheMatrix" object is given by "x" input.
## "cacheSolve" accepts "..." inputs, that will be passed to "solve()" function.
## For more information about "..." see "solve()" function help (?solve).
## Notice that if you want to compute the inverse of the same matrix with 
## different "..." options you should reboot the matrix, for example with:
## x$set(x$get())
cacheSolve <- function(x, ...) {
    # Get the stored inverse
    inv <- x$getinverse()
    # If this inverse is NOT NULL return the inverse stored in the environment.
    if(!is.null(inv)) {
        message("getting cached data")
        output <- inv
    }
    # If this inverse is NULL, a new inverse is calculated
    else{
        data <- x$get()
        inv  <- solve(data, ...)
        x$setinverse(inv)
        output <- inv
    }
    output
}






