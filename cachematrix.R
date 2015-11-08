# makeCacheMatrix function
# input: matrix
# output: list of 4 functions: get, set, getinv, setinv
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(
	  getinv = getinv, 
	  setinv = setinv, 
	  get = get, 
	  set = set
	 )
}

# Cachesolve function
# input: list from makeCacheMatrix
# output: matrix inverse
# trying to find cached solution first, if none - solve & cache
cacheSolve <- function(x, ...) {
  i  <- x$getinv()
  if (!is.null(i)) {
    message("Getting cached inverse")
    return(i)
  }
  m <- x$get()
  i <- solve(m)
  x$setinv(i)
  i
}
