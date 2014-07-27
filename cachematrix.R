## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Usage:
## M <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
## cacheMatrix <- makeCacheMatrix(M)
## cacheSolve(cacheMatrix)

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  set <- function(y) {
      ## Change the matrix being cached.
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function() x
      ## Returns the matrix being cached.
  setInverse <- function(inverse) cachedInverse <<- inverse
      ## Private function containing cached inverse of x
  getInverse <- function() cachedInverse
      ## Private function used to get the cached inverse of x
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invFunc <- x$getInverse()
  if(!is.null(invFunc)) {
    message("getting cached data")
    return(invFunc)
  }     ## Check if not null, return the cached inverse of x
  data <- x$get()
        ## If it is null, return the cached matrix x
  invFunc <- solve(data, ...)
        ## calculate inverse using solve function
  x$setInverse(invFunc)
  invFunc
}
