## The first function is used to create a special data type: a matrix, that
## caches its inverse. The second one can compute this inverse.



## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  
  set <- function(matr) {
    cache <<- NULL
    x <<- matr
  }
  get <- function() x
  
  setInverse <- function(inv) {
    cache <<- inv
  }
  getInverse <- function() {
    cache
  }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  
  if(!is.null(inverse))
    return(inverse)
  
  inverse <- solve(x$get())
  x$setInverse(inverse)
  
  inverse
}
