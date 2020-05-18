## Put comments here that give an overall description of what your
## functions do
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse
##  4. get the value of the inverse

## Write a short comment describing this function

##  Catching the Inverse of the Matrix

makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL
  get <- function() x
  getInverse <- function() {
    if(length(x)%%sqrt(length(x))==0) { ## make sure matrix is square
      inv <<- solve(x)
    }
  }
  list(get = get, getInverse = getInverse)
}

## computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) {
  if(!is.atomic(x)){      # check if the given parameter is atomic
    inv <- x$getInverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }     
  } else {        # if not atomic then make it
    message("getting the inverse-- no cached data found")
    return(makeCacheMatrix(x)$getInverse())
  } 
  
}
g<-makeCacheMatrix(matrix(1:4, 2, 2))
print(g)
