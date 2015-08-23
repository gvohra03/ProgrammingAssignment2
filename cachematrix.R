## The R script includes a pair of functions that cache the inverse of a matrix


## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i = NULL
  set = function(y) {
    x <<- y
    i <<- NULL
  }
  get = function() x
  setinv = function(solve) i <<- solve
  getinv = function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i = x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  mat.data = x$get()
  i = solve(mat.data, ...)
  x$setinv(i)
  return(i)
}
