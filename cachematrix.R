## Put comments here that give an overall description of what your
## functions do

## These functions will calculate the inverse of a matrix and cache it so it can be
## returned again without computation.

## Write a short comment describing this function

## Returns a list of functions that allow setting and retrieval of
## an input matrix and also allow setting and retrieval of 
## the cached inverse of that input.
makeCacheMatrix <- function(x = matrix(nrow=0, ncol=0)) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function

## Checks to see if a cached result exists, if so returns it.
## If not, it calculates the inverse, stores it as cache and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}