## Put comments here that give an overall description of what your
## functions do

## Function to create a matrix object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
    
}


## Compute the inverse of the makeCacheMatrix. Note that if X is a square 
## invertible matrix, then solve(X) returns it's inverse

cacheSolve <- function(x, ...) {
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- matrix(data, ...)
    x$setmatrix(m)
    solve(m)
    ## Return a matrix that is the inverse of 'x'
}
