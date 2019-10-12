## Pair of functions that needed to pass this course! And they used to create a special object that stores a matrix and caches its inverse.

## This one creates a "matrix" object which cache it's inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL

  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x

  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i

  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Second one. It computes the inverse of "matrix" created by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if (!is.null(i)) {
    return(i)
  }

  mat <- x$get()
  i <- solve(mat, ...)

  x$setInverse(i)
  i
}