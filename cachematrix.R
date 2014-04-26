## Set of Functions which allows to calculate inverse of the matrix.
## In order to optimize performance, matrix is cached once it's inverted, subsequent calls will return the cached copy
## makeCacheMatrix creates a list of functions to store original matrix and its inverse
## cacheSolve calculates inverse based on the list of functions provided from makeCacheMatrix
## It is expected that matrix is inevrsable.

## Create a list of functions that allows to store matrix and its inverse
## set, get - stores and return the original matrix
## setsolve, getsolve - stores and returns inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Return a matrix which is inverse of the matrix referenced by the list 'x'.
## Cached inverse matrix will be returned if cache exists in the list

cacheSolve <- function(x, ...) {
    ## retrieve the cached value first
    s <- x$getsolve()
    if(!is.null(s)) {
        ## If inversed matrix is in the cache, return it
        return(s)
    }
    
    ## get original matrix from the list and inverse it
    data <- x$get()
    s <- solve(data, ...)
    
    ## set inverse of the matrix to cache
    x$setsolve(s)
    s
}