## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve retrieves the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ##creae empty variable that will cache the inversed matrix
  set <- function(y) {
    x <<- y ## assigns x the value of the input matrix y passed as parameter by the function cacheSolve()
    inv <<- NULL ## empties the content of the previous inverse matrix
  }
  get <- function() x ## returns the current x matrix
  setinv <- function(solve) inv <<- solve ## assigns inv the inverse matrix passed as parameter by the cacheSolve() function
  getinv <- function() inv ## returns the current inverse matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) ## the list stores the 4 functions that have been defined together with their names
}

cacheSolve <- function(x, ...) {
  inv <- x$getinv() ## calls the function getinv() for the matrix x, stored in the list returned by makeCacheMatrix() function
  if(!is.null(inv)) { ## checks if matrix inv returned by getinf() is null or not
    message("getting cached data") ## if the inversed matrix inv for input matrix x has a content, prints a message
    return(inv) ## returns matrix inv
  }
  data <- x$get() ## calls function get(), stored in the list returned by makeCacheMatrix(), to get the matrix x that will be inversed
  inv <- solve(data, ...) ## inverses the provided matrix
  x$setinv(inv) ## calls function setinv() stored in the list returned by the function makeCacheMatrix() and passes the inversed matrix
  inv ## returns the inversed matrix
}
        ## Return a matrix that is the inverse of 'x'