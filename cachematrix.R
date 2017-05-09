## Put comments here that give an overall description of what your
## functions do

## Make list with setter and getter to inverted value
makeCacheMatrix <- function(x = matrix()) {
  # initialize cache store for inverse matrix
  inverse <- NULL
  set <- function(y) {
    x <<- y
    # empty cahce store
    inverse <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inverse <<- inv
  getinv <- function() inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Compute inverse for a matrix, if has stored value, return it instead
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinv(inverse)
  inverse
}
