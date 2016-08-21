
## Assuming that the matrix is always invertible. Below we have two functions
## that store a matrix and chaches its inverse

## the function makeCacheMatrix  creates the special matrix.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The following function calcutes the inverse of the matrix created in the makeCacheMatrix 
## It first checks if the inverse has already calculated (via the if condition)
## Otherwise it calculated the inverse

cacheSolve <- function(x, ...) {
       
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
