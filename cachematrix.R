## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix solves the inversion of the given matrix x and stores the solution

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


## cacheSolve looks up the solution to the inverse matrix and if doesn't exist, solves it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}