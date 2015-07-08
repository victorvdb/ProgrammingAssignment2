## The combination of these functions allow a user to invert a matrix and cache the results.
## As computation of inversions can take a long time, caching can speed up results when 
## User is in need of multiple inversions in another place

## The function makeCacheMatrix returns an object with 4 functions attached
## These functions are:
##
## set()                - to set a new matrix and reset the cache
## get()                - to return the original matrix
## setsolve()           - to set a new inverted matrix in the cache
## getsolve()           - to get the inverted matrix from cache

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)        
}


## The cacheSolve function takes an object created with the makeCacheMatrix as an argument.
## It will then first try to fetch the solved inverted matrix from cache, and return it if it is there
## If not, it will solve the matrix with the solve function and store it in cache using the setsolve function

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if (!is.null(s)) {
                message("Getting cached inverted matrix")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
