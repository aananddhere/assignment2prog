## These functions are part of programming assignment 2

## This function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        invmatrix <- NULL
    ## set caches the matrix value
            set <- function(y) {
              x <<- y
              invmatrix <<- NULL
        }
    ## get returns the cache matrix
        get <- function() x
        setInverse <- function(inverse) invmatrix <<- inverse
        getInverse <- function() invmatrix
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Returning a matrix that is the inverse of 'x'
      inv <- x$getInverse()
  ## checking if inverse has already been calculated'
      if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setInverse(inv)
      inv
}
