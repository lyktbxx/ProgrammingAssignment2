## function MakeCacheMatrix creates a matrix entity and loads it into the cache. 
## New Values can be pushed into the matrix by the function setinv
## cacheSolve returns the inverse of whatever matrix was loaded into it by
## function makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function updates the value of the inverse of a matrix if 
## the value of the matrix is updated.

cacheSolve <- function(x, ...) {
   m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m     
        ## Return a matrix that is the inverse of 'x'
}
