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

## Load library with definition for ginv() which will
## be used to calculate inverse of non-square matrix.
library(MASS)

## Returns a list with set/get/setInverse/getInverse
## functions for a matrix which is scoped lexically.
makeCacheMatrix <- function(x = matrix()) {
    ## Variable to hold inverse.
    inv <- NULL
    ## Assignment function
    set <- function(z) {
        x <<- z
        ## reset inverse value when matrix is modified.
        inv <<- NULL
    }
    ## returns the associated matrix.
    get <- function() x
    ## sets value to the inverse variable. 
    setInverse <- function(inverse) inv <<- inverse
    ## returns value stored in inv variable.
    getInverse <- function() inv
    ## Returns a list of methods.
    list(set=set, get=get,
         setInverse=setInverse,
         getInverse=getInverse)
}


## Returns inverse of a matrix based on whether its
## a square matrix or not. Once calculated, the inverse
## is cached and returned in subsequent calls to the
## function

cacheSolve <- function(x, ...) {
    ## fetch value in cache
    inv <- x$getInverse()
    ## if value found, return it.
    if (!is.null(inv)) {
        message("Returning cached inverse.")
        return(inv)
    }
    ## Well if we are here then no value was found.
    ## let's calculate the inverse. First get the matrix.
    message("Calculating inverse")
    data <- x$get()
    ## get matrix's dimensions to determine
    ## whether a square matrix.
    dimension = dim(data)
    ## Check for square matrix.
    if(dimension[1] == dimension[2]){
        message("Matrix is square hence computing inverse using solve.")
        ## Not all matrices are invertible. Hence we put
        ## solve in exception handle.
        tryCatch({
            inv <- solve(x)
        },
        error=function(cond) {
            ## Solve threw error while calculating inverse.
            ## lets default to our reliable ginv.
            message("Error occured while calculating inverse using solve.")
            message("using ginv to calculate inverse instead.")
            inv <<- ginv(data)
        })
    }
    else {
        ## Well not a square matrix. That makes things easy.
        message("Matrix is not square hence computing inverse using ginv.")
        inv <- ginv(data)
    }
    ## Let's cache the inverse for future reference.
    x$setInverse(inv)
    ## and also spit it out as result of the function.
    inv
}
