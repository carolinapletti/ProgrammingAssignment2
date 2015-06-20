## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.


makeCacheMatrix <- function(x = matrix()) {
  ## creates a list of functions that
  ## set the value of the matrix
  ## get the value of the matrix
  ## set the value of the inverted matrix
  ## get the value of the inverted matrix
    i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverted <- function(inverted) i <<- inverted
  getinverted <- function() i
  list(set = set, get = get, setinverted = setinverted, getinverted = getinverted)
}


cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  ## if the inverse of 'x' has already been cached, it uses the cached data and skips the computations
  i <- x$getinverted()
  if (!is.null(i)){
    message ("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverted(i)
  i
}
