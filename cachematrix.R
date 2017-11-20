
## Create the matrix and helper functions
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Helper function to set matrix data
  set <- function(y) {
    x   <<- y
    inv <<- NULL
  }
  
  # Helper function to get matrix data.
  get <- function() {
    x
  }
  
  # Helper function to set matrix inverse.
  setinv <- function(inverse) {
    inv <<- inverse
  }
  
  # Helper function to get matrix inverse.
  getinv <- function() {
    inv
  }
  
  # Return the list with objects to helper functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  # Get the cached inverse.
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  
  # Inverse not cached, get the matrix values.
  data <- x$get()
  
  # Calculate inverse.
  inv <- solve(data, ...)
  
  # Cache the object's inverse.
  x$setinv(inv)
  
  # Print the inverse on console.
  inv
}
