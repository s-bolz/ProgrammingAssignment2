## This script contains functions that implement a cache for the calculation of
## inversed matrices. The first function (makeCacheMartrix) returns a cache for
## an inversed matrix. The second function (cacheSolve) returns an inverse from
## that cache or - if necessary - calculates an inverse and stores it in the
## cache.

## The function makeCacheMatrix wraps a given matrix in a cache matrix object
## that offers functions to
##   - set a new matrix or get the wrapped matrix
##   - cache the inverse of the wrapped matrix or retrieve its cached inverse

makeCacheMatrix <- function(x = matrix()) {
    inversedMatrix <- NULL
    setMatrix <- function(newMatrix) {
        x <<- newMatrix
        inversedMatrix <<- NULL
    }
    getMatrix <- function() x
    setInversedMatrix <- function(newInversedMatrix) inversedMatrix <<- newInversedMatrix
    getInversedMatrix <- function() inversedMatrix
    list(
        setMatrix = setMatrix,
        getMatrix = getMatrix,
        setInversedMatrix = setInversedMatrix,
        getInversedMatrix = getInversedMatrix
    )
}


## The function cacheSolve calculates the inverse of a matrix which is wrapped
## by the cache matrix object which on its part is returned from the
## makeCacheMatrix function. If the cache already contains the inverse it is
## immediately returned instead of being calculated again, if not the inverse
## is calculated and stored in the cache.

cacheSolve <- function(x, ...) {
    inversedMatrix <- x$getInversedMatrix()
    if (!is.null(inversedMatrix)) {
       message("returning inversed matrix from cache")
       return(inversedMatrix)
    }
    message("did not find inversed matrix in cache, calculating inverse")
    matrix <- x$getMatrix()
    inversedMatrix <- solve(matrix, ...)
    x$setInversedMatrix(inversedMatrix)
    ## Return a matrix that is the inverse of 'x'
    inversedMatrix
}
