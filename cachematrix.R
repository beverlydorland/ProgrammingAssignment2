
## (Used Git Bash Desk top app to clone file, set working directory to DataScience.)

## This file contains two functions which cache a matrix and the inverse of the 
## matrix, rather than repeatedly computing the inverse when the matrix does not
## change. After the inverse is computed, we save cpu time by using the cached object. 

## This function takes a matrix as an argument and returns a list of functions 
## callable from the global environment. Each function has it's own environemnt, local
## scope.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) s <<- solve
    getSolve <- function() s
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)

}
## This function checks to see if the matrix inverse has been computed yet; 
## if so, the inverse is returned and the function is completed/exited.
## If the matrix inverse has not been computed, then the code after the "if" 
## statment computes the inverse of the matrix stored in x$get() and returns
## the inverse of the matirx. NOTE: x$getSolve, x$setSolve and x$get are sought in 
## the function environment in which they were created.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getSolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setSolve(s)
    s
}
