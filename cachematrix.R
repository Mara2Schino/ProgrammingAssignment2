## makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse. It will have the following functions:
## set - set the value of the matrix
## get - get the value of the matrix
## setInverse - calculate the inverse of the matrix and cache it
## getInverse - get the value of the matrix inverse
##
## -------Sample Output---------------
##> source("cachematrix.R")
##> matrixA <- makeCacheMatrix()
##> matrixA$set(matrix(1:4, 2, 2))
##> cacheSolve(matrixA)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(matrixA)
##getting cached data
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve(x) #get the matrix inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}