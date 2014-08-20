## The function caches the inverse of a matrix. The two functions below are used to create a
## special object that stores a matrix and caches its inverse. 


## This function creates a list, which (i) sets the value of the matrix, (ii) gets the matrix
## (iii) sets the inverse of the matrix, (iv) gets the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The following function solves for the inverse of the matrix in the list created by the 
## above function. It first checks whether the inverse was already solved for. If so, it gets
## the inverse from the cache and skips the computation. Otherwise, it calculates the inverse
## of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
