## The two functions makeCacheMatrix and cacheSolve are useful when 
## calculating the inverse of very large matrices, which can be a
## computationally expensive process. Used together they save time
## by checking whether or not the inverse of matrix M already
## exists and, if so, retrieving it from the cache. The use of the
## <<- operator is important. Specifically, it allows the variables
## x and Minv to be stored outside of the functions in which they
## are defined

## makeCacheMatrix is a function that stores a list of functions

makeCacheMatrix <- function(M = matrix()) {
        Minv <- NULL
        ## the set function below substitutes x with y (the input) 
        ## in the main function (as opposed to only in the set function)
        ## It also restores mInv to null
        set <- function(y) {
                x <<- y
                Minv <<- NULL
        }
        ## the get function below returns the matrix M stored in the
        ## main function
        get <- function() M
        ## the following functions only set/get the inverse of the
        ## matrix, they do not perform any calculations
        setinverse <- function(inverse) Minv <<- inverse
        getinverse <- function() Minv
        ## the following expression stores the 4 functions within
        ## the function makeCacheMatrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Input to cacheSolve is the object where makeCacheMatrix is stored

cacheSolve <- function(M, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Get the existing matrix M
        Minv <- M$getinverse()
        ## If M is not null, retrieve existing inverse of M
        if(!is.null(Minv)) {
                message("getting cached data")
                ## Return the inverse M'
                return(Minv)
        }
        ## Otherwise get M
        data <- M$get()
        ## Calculate the inverse of M
        Minv <- solve(data, ...)
        ## Set/store the inverse of M in the cache
        M$setinverse(Minv)
        ## Return the inverse of M
        Minv
}