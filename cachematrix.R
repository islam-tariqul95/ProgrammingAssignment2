## Put comments here that give an overall description of what your
## functions do

## Create a special matrix that caches it's inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inv) inverse <<- inv
  
  getInverse <- function() inverse
  
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## Set or Get Inverse matrix to/from cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inverse)
  }
  
  mtrx <- x$get()
  inv <- solve(mtrx)
  x$setInverse(inv)
  inv
}
