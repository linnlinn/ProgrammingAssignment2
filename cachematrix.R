## These 2 functions calculate the inverse of a square matrix. First they check if the inverse matrix has 
## already been calculated earlier and stored in the memory. If it is the case, the value is simply restored.
## Syntax : run myList<-makeCacheMatrix(Z) where Z is a square matrix, then run cacheSolve(myList) to get 
## the inverse matrix of Z (cached or calculated).

## makeCacheMatrix creates a list of 4 functions : to store and to restore a matrix, to calculate 
## the inverse matrix and to get the inverse matrix from cache 

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



## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
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
