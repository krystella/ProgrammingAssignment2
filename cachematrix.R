## Assignment: Caching the Inverse of a Matrix
## The following functions use lexical scoping to cache the inverse of a matrix
## Caching the inverse of a matrix is beneficial as it saves from having the inverse
##    computed repeatedly

## This function creates a special "matrix" object that can cache its inverse
## This function:
##      1. Sets the value of a matrix
##      2. Gets the value of a matrix
##      3. Sets the value of the inverse
##      4. Gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function returns the inverse of the special "matrix" returned by makeCacheMatrix
## The function first checks if the inverse has already been calculated, 
##    If yes, then returns the inverse from the cache
##    Else, computes the inverse of the matrix

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        mat <- x$get()
        i <- solve(mat, ...)
        x$setinverse(i)
        i
}
