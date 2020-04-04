## a pair of functions that cache the inverse of a matrix

## function:creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL #initialize the cache
  set <- function(y) {
    #when the matrix is set, clear the cache
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse #set the cache
  getInverse <- function() inv
  
  #returns a list with 4 functions
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## function:computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #check if the inverse has already been calculated
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached inverse data")
    return(inv)
  }
  
  #call solve function to calculate the inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  #set inverse data to the cache
  x$setInverse(inv) 
  inv
  
}
