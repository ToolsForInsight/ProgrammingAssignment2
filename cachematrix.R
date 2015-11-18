# makeCacheMatrix: This function creates a special
# "matrix" object that can cache its inverse.
# For this to work, the matrix supplied must be invertible.
# Use cacheSolve to calculate and cache the inverse of the matrix.
# in the created special matrix object.
# However, if set() is used, the cached inverse will
# be removed (as the matrix has changed).
# NOTE: Intended functionality will break if the argument
# supplied to the makeCacheMatrix function or set() function
# is not a matrix.
#
# cacheSolve: This function computes the inverse of the
# special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the
# matrix has not changed), then the cacheSolve
# retrieves the inverse from the cache.
# NOTE: This function will produce an error if the matrix
# in the "special" matrix object supplied is not invertible.
# This appears to be "OK" per the assignment instructions.

## Create a "special" matrix object
makeCacheMatrix <- function(x = matrix()) {
  xInverse <- NULL
  set <- function(y) {
    x <<- y
    xInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseMatrix) xInverse <<- inverseMatrix
  getInverse <- function() xInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Return a matrix that is the inverse of 'x'
## Note 'x' is a "special matrix object" returned by makeCacheMaktrix
cacheSolve <- function(x, ...) {
  xInverse <- x$getInverse()
  if(!is.null(xInverse)) {
    message("getting cached data")
    return(xInverse)
  }
  data <- x$get()
  
  xInverse <- solve(data)
  x$setInverse(xInverse)
  xInverse
}
