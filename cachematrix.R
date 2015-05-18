# Below are two functions that are used to create a list of functions
# that stores a matrix and to cache's it's inverse. 

#The first function makeCacheMatrix creates a list of functions to 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
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

# The second functions cacheSolve computes the inverse of matrix stored by
# makeCacheMatrix. First, cacheSolve uses getinverse to varify if the inverse has
# already been computed. If the inverse has already been calculated, 
# then the inverse is retrieved from the cache and not calculated.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  else {
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
  }
}
