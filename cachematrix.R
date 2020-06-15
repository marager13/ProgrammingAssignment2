##This pair of functions cache and compute the inverse of a matrix.

# This function creates the matrix object that will be able to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m 
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## This function computes the inverse of the matrix returned by 
#makeCacheMatrix above. If the inverse has already been calculated 
#then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mdata <- x$get()
  minverse <- solve(mdata, ...)
  x$setinv(minverse)
  return(minverse)
}


