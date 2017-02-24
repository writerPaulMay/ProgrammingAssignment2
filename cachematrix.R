## Two functions which cache the inverse of a matrix.

## Student: writerPaulMay

## makeCacheMatrix
## Creates a special matrix object that can cache its inverse.
## Returns a list of functions to get and set the matrix,
## and to get and set the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve
## Returns the inverse of the special matrix object returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed, 
## retrieve the inverse from the cache; otherwise compute the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
