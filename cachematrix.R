## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverted <- function(inverted) inv <<- inverted
  getinverted <- function() inv
  list(set = set, get = get,
       setinverted = setinverted,
       getinverted = getinverted)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinverted()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverted(inv)
  inv
}
