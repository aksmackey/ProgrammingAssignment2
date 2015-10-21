## These functions calculate the inverese of a matrix and store it in the 
## cache for accessabilty. 

## makeCacheMatrix is a list of functions that set the value of the matrix,
## get the value of the matrix, set the value of the inverse, and get the 
## value of the inverse. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calcuates the inverse of the matrix described in makeCacheMatrix
## if the inverese has not already been calculated and stored in the
## cache. If it has already been calculated, it gets the inverse from 
## the cache and does not recaculate. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
