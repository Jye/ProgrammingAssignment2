## Put comments here that give an overall description of what your
## functions do

##Create a matrix whose inverse can be cached
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  ##Assigns a new matrix and invalidates the inverse cache
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  setInverse <- function(i) inverse <<- i
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Returns the cached inverse of matrix 'x'. If inverse of 'x' is not cached, it is calculated and cached.
cacheSolve <- function(x, ...) {
  
  ##Calculate and cache inverse if necessary
  inverse <- x$getInverse()
  if (is.null(inverse)) {
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
  }
  
  x$getInverse()
}
