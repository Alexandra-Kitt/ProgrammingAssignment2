##### Programming Assignment 2 Description
## makeCacheMatrix and cacheSolve work together to find the inverse of a matrix
## in the most efficient way possible, whereby when cacheSolve is called, rather
## than performing tedious calculations cacheSolve first checks with 
## makeCacheMatrix to see if the inverse has already been calculated. This is 
## possible through the <<- assigning operator, which assigns values so that R
## can find them through the parent function (so that the cacheSolve function
## can search for the functions get, getsolve, etc. within the makeCacheMatrix).

# makeCacheMatrix: first, an empty matrix is constructed, followed by the
# functions that cacheSolve requires. Finally, a list is made of the functions
# so that they are named, and can thus be called by cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
  set <- function(y) {
      x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inverse <<- solve
  getsolve <- function() inverse
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


# cacheSolve(): Before solving (inverting) the matrix, cacheSolve calls on 
# makeCacheSolve using x$getSolve() and checks to see whether or not the value
# of the mat_rix is equal to NULL (ie, the data has not yet been cached). If the
# data is there, cacheSolve returns the result cached. If not, cacheSolve inverts
# the input matrix and also stores it in the cache via setsolve().

cacheSolve <- function(x, ...) {
  mat_rix <- x$getsolve()
  if (!is.null(mat_rix)) {
    message("getting cached data")
    return(mat_rix)
  }
  data <- x$get()
  mat_rix <- solve(data)
  x$setsolve(mat_rix)
  mat_rix
}