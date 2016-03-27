## The functions makeCacheMatrix and cacheSolve allow a matrix to be stored, and its inverse
## computed when required and simultaneously cached to avoid computing again upon further requests.



## The function makeCacheMatrix creates a list of functions for a matrix argument:
##
## set - Sets the matrix to be stored and sets 'm' as NULL to indicate a change of matrix
## get - retrieves the original matrix
## setsolve - sets the inverse of the matrix upon request
## getsolve - retrieves the inverse of the matrix upon request

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function()x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)

}

## The function cacheSolve takes a list created by makeCacheMatrix as an argument and either
## computes the inverse of a matrix, or returns the cached inverse of a matrix if available.
##
## The function makeCacheMatrix returns NULL for getsolve() upon initialising a new matrix,
## so cacheSolve moves to the latter part of the function where the matrix is retrieved
## and then 'm' is set as the matrix inverse. If the function cacheSolve is run again on the
## same argument then getsolve() will return the matrix inverse to 'm', and the if statement
## is entered. This simply returns m rather than computing the matrix inverse again.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setsolve(m)
        m
        
        ## Return a matrix that is the inverse of 'x'
}
