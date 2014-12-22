
## This program cachematrix.R creates a pair of functions makeCacheMatrix & cacheSolve that cache the inverse of a matrix.
## Matrix inversion can be a costly computation especially for large matrices and there may be some benefit to caching the 
## inverse of a matrix rather than compute it repeatedly  

## This function makeCacheMatrix creates a custom matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list (set=set,get=get,
        setinv=setinv,
        getinv=getinv)
}


## This function cacheSolve computes the inverse of the custom matrix returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting matrix inverse cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}


