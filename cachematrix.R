## Put comments here that give an overall description of what your
## functions do

# These functions stores perhaps costly computations in cache, so that we can avoid doing the same
# calculation multiple times.
# The functions use the <<- operator to assign values to objects in a different environment than the current one.

## Write a short comment describing this function

# The function makeCacheMatrix() creates a special "matrix" which is actually a list of functions.
# The list contains four functions:
# set(): Set the value of the matrix
# get(): Get the value of the matrix
# setinverse(): Set the value of the inverse of the matrix
# getinverse(): Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(mat) inv <<- mat
  
  getinverse <- function() inv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

# The function cacheSolve() calculates the inverse of the special "matrix" created with the function above.
# The getinverse() function is called. If this returns a not-null value, then the inverse has already
# been calculated and its value is returned from cache. This means that we can skip the perhaps costly 
# calculation, beacuse we have done it previously. 
# If the inverse has not yet been calculated (i.e. if the getinverse() function returns a null), 
# the calculation is done, and the result is placed in the cache via the setinverse() function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}



