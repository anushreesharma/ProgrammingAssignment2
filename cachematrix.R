## Put comments here that give an overall description of what your
## functions do

## makeCacheMatric function helps to create a 'special' matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(I) inverse <<- I
  getInverse <- function() inverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## The function below is responsible for 
##calculating the inverse of matrix created
##using makeCacheMatrix.If inerse already exists for a given matrix 
##it simply fetches the cachecd inverse.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("fetching cached data")
    return(inverse)
  }
  mat <- x$get()
  inverse <- solve(mat, ...)
  x$setInverse(inverse)
  inverse
}



