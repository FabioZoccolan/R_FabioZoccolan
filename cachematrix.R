## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a matrix object which cashes
## its inverse for the input 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInv <- function(inv) i <<- inv
  getInv <- function() i
  list(set = set, 
       get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve is a function which computes the inverse of matrix
## returned by makeCacheMatrix function above. 
## If the inverse has already been calculated without changing the matrix,
## then cacheSolve function should recover the inverse from the cache


cacheSolve <- function(x, ...) {
    i <- x$getInv()
    if(!is.null(i)) {
      message("Getting inversed matrix")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInv(i)
    i
}

