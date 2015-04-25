## The following functions create a mechanism to cache 
## inverse of a matrix.
## makeCacheMatrix creates a list with set/get/setInvers/
## getInverse functions to get/set matrix and it's 
## inverse.
## cacheSolve returns inverse of a matrix from cache
## if it was calculated previously else it calculates
## the inverse using solve for square matrix and ginv
## for non-square matrix. If solve fails, it defaults
## to ginv.

## Load library with definition for ginv()
library(MASS)

## Returns a list with set/get/setInverse/getInverse
## functions for a matrix.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(z) {
        x <<- z
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set=set, get=get,
         setInverse=setInverse,
         getInverse=getInverse)
}


## Returns inverse of a matrix based on whether its
## a square matrix or not. Once calculated, the inverse
## is cached and returned in subsequent calls to the
## function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("Returning cached inverse.")
        return(inv)
    }
    data <- x$get()
    message("Calculating inverse")
    dimension = dim(data)
    if(dimension[1] == dimension[2]){
        message("Matrix is square hence computing inverse using solve.")
        tryCatch({
            inv <- solve(x)
        },
        error=function(cond) {
            message("Error occured while calculating inverse using solve.")
            message("using ginv to calculate inverse instead.")
            inv <<- ginv(data)
        })
    }
    else {
        message("Matrix is not square hence computing inverse using ginv.")
        inv <- ginv(data)
    }
    x$setInverse(inv)
    inv
}
