## Functions use to efficiently calculate the inverse of a matrix.
## Caching is used to decrease the cost of repeat calculations.

## makeCacheMatrix takes a matrix and returns a list of function objects.
## The functions include get, set, setinverted, and getinverted. get and set
## get and set the base matrix. getinverted and setinverted get and set
## the matrix inverted. There is no validation to ensure that a inverted
## matrix set with setinverted is mathematically correct.

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


## cacheSolve is used with a list object returned from makeCacheMatrix.
## If the inverted matrix has not been calculated before, it calculates it
## caches it, and returns it. If it has been calculated before, it
## returns the cached copy.

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
