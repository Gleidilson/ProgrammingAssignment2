##### Programming Assignment 2 #####


# The function makeCacheMatrix will create a list wich contains a function to:
# Set the value of the matrix.
# Get the value of the matrix.
# Set the value of inverse of the matrix.
# Get the value of inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL
  set <- function(w) {
    x <<- w
    inv_m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv_m <<- inverse
  getinv <- function() inv_m
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


# The function cacheSolve will...
# Return the inverse of the matrix. 
# Check if the inverse has been computed.
# If it has: it gets the result and skips the computation.
# If it hasn't: it computes the inverse and then sets the value in the cache.

cacheSolve <- function(x, ...) {
  inv_m <- x$getinv()
  if(!is.null(inv_m)) {
    message("cached data:")
    return(inv_m)
  }
  data <- x$get()
  inv_m <- solve(data)
  x$setinv(inv_m)
  inv_m
}
