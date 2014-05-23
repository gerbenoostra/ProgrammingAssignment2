## Returns a matrix for usage with cacheSolve, allowing the solve result to be cached
## The returned list contains the following elements:
## set : to replace the matrix (resets the cached inverse matrix)
## get : to get the internal matrix (without cache)
## setinverse : to store the corresponding inverse matrix
## getinverse : to get the corresponding inverse matrix (NULL if never set)
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of matrix x, using the solve {base} method.
## If the inverse has already been calculated, the cached result will be returned
## Argument x needs to be of the type returned by makeCacheMatrix
## For further arguments see the help page of 'solve'
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) { #if cache available
    message("getting cached data")
    return(m)
  }#else: if cache not available
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
