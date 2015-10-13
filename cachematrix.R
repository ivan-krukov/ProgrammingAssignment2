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
  self.inverse <- NULL
  
  self.set <- function(y) {
    if (is.matrix(y)) {
      x <<- y
      self.inverse <- NULL
    } else {
      stop("makeCacheMatrix$set: The provided object is not a matrix")
    }
  }
  self.get <- function() x
  self.setInverse <- function(inverse) {
    self.inverse <<- inverse
  }
  self.getInverse <- function() self.inverse
  return(list(
    set = self.set,
    get = self.get,
    setInverse = self.setInverse,
    getInverse = self.getInverse
  ))
}


## Calculate the inverse of the matrix object created with `makeCacheMatrix()`, store the result.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  self.inverse <- x$getInverse()
  if (!is.null(self.inverse)) {
    message("getting cached inverse")
    return(self.inverse)
  }
  self.data <- x$get()
  self.inverse <- solve(self.data, ...)
  x$setInverse(self.inverse)
  return(self.inverse)
}
