## Generate a special 'matrix', and cache its inverse matrix. 
## If the inverse matrix is already in the cache, just retrieve it and do not do any computation.

## makeCacheMatrix function is used to generate a special 'matrix' and cache its inverse matrix,
## if the inverse matrix is exist.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x      ## get the matrix x 
  setInverse <- function(solve) inv <<- solve
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## If the inverse matrix of input is in the cache, return it, otherwise get the inverse matrix and cache it.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()     ## query the matrix x's cache
  if (!is.null(inv)) {      ## check if there is a cache
    message("getting cached data")
    return(inv)             ## return the cache if it's exist, don't do computation
  }
  data <- x$get()           ## get the matrix x to prepare for computation
  inv <- solve(data, ...)   ## compute the inverse matrix of x
  x$setInverse(inv)         ## save the result back to x's cache
  inv                       ## return the result inverse matrix
}
