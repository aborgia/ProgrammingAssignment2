## This program takes in a matrix "x" and creates and caches the inverse of it. The result is 
## a matrix. This only works for invertible matrices.

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    mat <- NULL
    set <- function(y) {
      x <<- y
      mat <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) mat <<- solve
  getinverse <- function() mat
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#### This function returns a matrix, "mat", that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        
  mat <- x$getinverse()
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data, ...)
  x$setinverse(mat)
  mat
}
