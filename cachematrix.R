## the following two functions allow for the computation
## of a matrix inverse and cache the result for 
## future use so that solve() does not have to be
## called on a particular matrix more than once


## makeCacheMatrix accepts a matrix and returns a list
## of functions to get and set the data matrix
## and the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve takes as input an object created by
## makeCacheMatrix and computes the inverse or retrieves
## the cached inverse from a prior computation

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
