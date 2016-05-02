# 02.Wk3 - Assignment 2
# Caching the Inverse of a Matrix
# Your assignment is to write a pair of functions that cache the inverse of a
# matrix.

# The following solutions are based on the examples given in course assigment.
## https://www.coursera.org/learn/r-programming/peer/tNy8H/programming-assignment-2-lexical-scoping

## 1. makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.
makeCacheMatrix <- function(x = martix()){
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# 2. cacheSolve: This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then the cachesolve should retrieve the
# inverse from the cache.
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("Getting cached data...")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

# To test, source the above code and then run the following to create a test
# matrix:
testMatrix <- matrix(c(1:4),2,2)

# Next, create an object to hold the special "matrix" object from (1) above:
mCm <- makeCacheMatrix(testMatrix)

# Finally, call cacheSolve with that object:
cacheSolve(mCm)

# To see the notification that data is coming from cache, repeat the above
# command:
cacheSolve(mCm)
