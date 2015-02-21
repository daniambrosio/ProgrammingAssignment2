## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly. These two functions below handle the caching creation and 
## the process of inverting the matrix. The functions assume that the 
## matrix supplied is always invertible.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        ## this will clear all the data in the scope in case a new matrix 
        ## is submitted
        x <<- y
        s <<- NULL
    }
    ## returns the matrix
    get <- function() x
    ## set the solve function in the scope
    setsolve <- function(solve) s <<- solve
    ## get the solve function from the scope
    getsolve <- function() s
    ## create and make the list ready to be returned (including the accessor
    ## methods)
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)    
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.
cacheSolve <- function(x, ...) {
    ## get the solve method (responsible for inverting the matrix)
    s <- x$getsolve()
    if(!is.null(s)) {
        ## if cached data is found, return it, without re-calculating
        message("getting cached data")
        return(s)
    }
    ## get the matrix
    data <- x$get()
    ## invert the matrix...
    s <- solve(data, ...)
    ## ... and store its inversed matrix in the scope
    x$setsolve(s)
    ## Return a matrix that is the inverse of 'x'
    s
}
