## Two functions that create a special object that stores a invertible matrix 
## and cache's its inverse.

## Creates a special "matrix", caches matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<-y
    i <<- NULL
  }

  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  } else {
    matrix <- x$get()
    i <- solve(matrix, ...)
    x$setinverse(i)
    i
  }
}
