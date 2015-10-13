## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  self.inverse <- NULL
  
  self.set <- function(y) {
    x <<- y
    self.inverse <- NULL
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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  self.inverse <- x$getInverse()
  if (!is.null(self.inverse)) {
    message("getting cached inverse")
    return(self.inverse)
  }
  self.data <- x$get()
  self.inverse <- solve(self.data)
  x$setInverse(self.inverse)
  return(self.inverse)
}
