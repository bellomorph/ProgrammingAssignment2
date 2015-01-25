## The below functions compute and cache the inverse of a matrix using
## the <<- operator. 

## The following function creates a special "matrix" object that can cache
## its inverse. The function actually returns a list with four functions:
##   1. Get the value of the matrix. 
##   2. Set the value of the matrix.
##   3. Get the value of the inverse matrix.
##   4. Set the value of the inverse matrix. 
##  Because these functions were defined inside makeCacheMatrix,
##  they have access to the value of the matrix passed to makeCacheMatrix, 
##  which is assumed to be invertible. If no argument is given, the default
##  matrix is used. 
 
makeCacheMatrix <- function(x = matrix()) {
  ## Return a matrix that is the inverse of 'x'
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function takes as its argument the "matrix" object
## returned by makeCacheMatrix, calculates its inverse, stores it in the cache, 
## and returns it. If the inverse has already been calculated, and 
## the matrix not changed since, then the inverse is retrieved from the cache. 

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
