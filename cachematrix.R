## Author: Jeff Noel
## Created: Jan 25, 2015
## Edited: Jan 25, 2015

## Create a vector containing functions to get and set the entries of a matrix,
##   as well as functions to get and set the defined value of its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    ## matrix's inverse not defined until cacheSolve is called
    xInverse <- NULL
    
    ## Sets the matrix's entries to the argument. Inverse will become undefined.
    set <- function(y) {
        x <<- y
        xInverse <<- NULL
    }
    
    ## return the entries of the matrix
    get <- function() x
    
    ## Used by cacheSolve to store the marix's inverse
    setInverse <- function(inverse) xInverse <<- inverse
    
    ## return the stored value of the matrix inverse (or NULL)
    getInverse <- function() xInverse
    
    ## packages together the defined functions into an output
    list (set = set, get = get, setInverse = setInverse,
          getInverse = getInverse)
  
}


## Takes the matrix structure defined in makeCacheMatrix and outputs the
## matrix's inverse. Initially, it computes this via solve, but subsequently
## uses the value computed in previous calls.

cacheSolve <- function(x, ...) {
   
    
    ## If the inverse was already cached, return it.
    xInverse <- x$getInverse()
    if(!is.null(xInverse)) {
        message("Using cached matrix inverse.")
        return (xInverse)
    }
    
    ## Otherwise, get the matrix entries and compute the inverse via solve().
    ## Save the result of this computation within the matrix structure.
    matrixEntries <- x$get()
    inverse <- solve(matrixEntries, ...)
    x$setInverse(inverse)
    inverse
}