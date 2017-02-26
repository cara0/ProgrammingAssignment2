## Programming Assignment 2: caching the inverse of a matrix
## This program has two functions:
## 1. makeCacheMatrix 
##      - takes a matrix as input, returns a cache-able matrix object.
## 2. cacheSolve 
##      - takes special matrix object
##      -returns inverse of the matrix 
##      (either by computing it or retrieving previously computed object)

## Write a short comment describing this function
## This function takes a matrix and creates a matrix object with the same values that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## This function takes the special matrix object created with makeCacheMatrix and either retrieves a cached inverse or computes the inverse of it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)){
                message('getting cached data')
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinv(m)
        m
}
