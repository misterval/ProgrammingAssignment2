## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function takes an invertible matrix as input and creates a list with 
## setter and getter functions to store the matrix and its inverse 
## if already solved in the cache
makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setminv <- function(invm) minv <<- invm
  getminv <- function() minv
  list(set = set, get = get,
       setminv = setminv,
       getminv = getminv)
}


## Write a short comment describing this function
## This function takes in a list describing an environment as output by the 
## makeCacheMatrix function and then solves for the inverse of the matrix. 
## If the inverse has already been calculated, it retrieves it from cache rather than 
## resolving for it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minv <- x$getminv()
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data, ...)
  x$setminv(minv)
  minv
}
