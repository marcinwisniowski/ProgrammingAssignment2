## Coursera: Data Science Specialization
## Module: R Programing
## Assignment: 2


## makeCacheMatrix
## This function creates a "Special" version of matrix, which stores its inverted
## version in cache. It provides easy interface to work with cache
makeCacheMatrix <- function(x = matrix()) {
    matrix_cache <- NULL

    setMatrix <- function(mat) {
        x <<- mat
        matrix_cache <<- NULL
    }
    getMatrix <- function() {
        x
    }
    setInverted <- function(mat) {
        matrix_cache <<- mat
    }
    getInverted <- function() {
        matrix_cache
    }

    list(
        getInverted = getInverted,
        getMatrix = getMatrix,
        setInverted = setInverted,
        setMatrix = setMatrix
    )
}

## cacheSolve
## This function is an facade to solve function, which uses cached matrix
## object and returns inverse matrix. If the invers doesn't exists this
## function calculates it in traditional way.
cacheSolve <- function(x, ...) {
    inverted <- x$getInverted()

    if ( is.null(inverted) ) {
        data <- x$getMatrix()
        if ( nrow(data) != ncol(data) ) {
            stop("Can not calculate inversion of a not square Matrix")
        }
        inverted <- solve(data, ...)
        x$setInverted(inverted)
    }

    inverted
}
