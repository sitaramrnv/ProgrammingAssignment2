## This file containes two functions enabling calculation of inverse matrix and also storing that in
## cache memory and returing the cached inv if available, there by avoiding calculations

## The function makeCacheMatrix creates a class with methods declared for get, set, getinv and setinv
## In  this class, the 'get' function returns the given matrix when called in the next class
## cacheSolve



makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
}


## This function takes the matrix object as input and calculates inverse if it is 
##not already available and returns that after storing in the object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  }

