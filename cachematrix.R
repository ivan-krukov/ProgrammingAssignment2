## NAME: 
##   cachematrix.R
## DESCRIPTION: 
##   This implements two functions for cached matrix inverse
## DETAILS:
##   `makeCacheMatrix(x)` creates a new object with the following methods:
##     `$set` sets the internally stored matrix
##     `$get` returns the internally stored matrix
##     `$setInverse` sets the inverse of the matrix
##     `$getInverse` returns the inverse of the matrix
##    Note that these are private methods, and should not be called separately
##   
##   `cacheSolve(x)` takes a matrix object created with `makeCacheMatrix()` and returns its inverse, caching the result. 
##    If the inverse has been calculated previously, it will not be re-calculated, but the cached inverse will be returned.
## USAGE:
##   ```
##   x <- makeCacheMatrix(matrix(runif(9), nrow = 3))
##   cacheSolve(x) # First solve - inverts the matrix, caches the result
##   cacheSolve(x) # Second solve - returns the cached result
##   ```

## Create a new cache matrix object
makeCacheMatrix <- function(x = matrix()) {
  # Note that I am prepending all the local variables with `self.` to avoid confusion.
  # This only syntactically emulates the object, no actual `self` instance exists.
  
  # The re-entrant state variable, actually caching the inverse
  self.inverse <- NULL
  
  # Set the underlying matrix data, checking that the data provide is a matrix
  self.set <- function(y) {
    if (is.matrix(y)) {
      x <<- y
      self.inverse <- NULL
    } else {
      stop("makeCacheMatrix$set: The provided object is not a matrix")
    }
  }
  
  # Get the underlying data
  self.get <- function() x
  
  # Set inverse of the object's matrix
  self.setInverse <- function(inverse) {
    self.inverse <<- inverse
  }
  
  # Get the cached inverse
  self.getInverse <- function() self.inverse
  
  # Return a method list
  return(list(
    set = self.set,
    get = self.get,
    setInverse = self.setInverse,
    getInverse = self.getInverse
  ))
}


## Calculate the inverse of the matrix object created with `makeCacheMatrix()`, store the result.
cacheSolve <- function(x, ...) {
  
  # Try getting the inverse of the object's matrix
  self.inverse <- x$getInverse()
  
  # If it was calculated, return the cached version
  if (!is.null(self.inverse)) {
    message("cacheSolve: using cached inverse")
  }
  
  # Otherwise, calculate the inverse and save it
  else {
    
    # Get the object's matrix
    self.data <- x$get()
    
    # Calculate the inverse, propagating all the parameters with `...`
    self.inverse <- solve(self.data, ...)
    
    # Cache the inverse
    x$setInverse(self.inverse)
  }
  return(self.inverse)
}
