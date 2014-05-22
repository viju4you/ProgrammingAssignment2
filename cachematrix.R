## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##object that can cache its inverse.
## It is return a list containing the functions
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
#if the matrix is not square the function is terminated
  if (nrow(x)!=ncol(x)) stop("The matrix is not square and it is not invertible")
  # initialize the stored inverse value to NULL
  inverse <- NULL
  # to set the value of the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL   # since the matrix changed
  }
  # to get the value of the matrix
  get <- function() x
  # to set the inverse
  setinv <- function(inv) inverse <<- inv
  # to get the inverse
  getinv <- function() inverse
  
  # return a list of all the above functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)    
}


## The following function calculates the inverse of the special 
## "matrix" created with the makeCacheMatrix function. 
## In the initial part it checks to see if the inverse 
## has already been calculated. 
## If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the 
## matrix and sets the value of the inverse in the cache via 
## the setinv function.


cacheSolve <- function(x, ...) {
 # check if the inverse is already cached
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # not cached, so we get the matrix into data
  data <- x$get()
  # and compute the inverse
  inverse <- solve(data, ...)
  # then cache the inverse
  x$setinv(inverse)
  # and return it as well
  inverse
}
