## The following functions calculate the inverse of a matrix
## and cache the result.

## This function creates a list containing a function to:
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## This function returns the inverse of a matrix by first
## checking the cache to see if it has already been computed.
## If it has already been computed, it skips the computation
## and uses the value stored in cache.  If it has not been
## computed previously, it is computed and added to cache.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    
    if(!is.null(i)) {
        message("It is cached")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
