# The following set of functions enable the creation of a special matrix object.
# This matrix object will cache its inverse once required to compute it a first time.
# As computing the inverse of a matrix is computationally intensive, 
# such a feature may come in very handy.




# 'makeCacheMatrix' function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL
  
  set <- function(y) {
    x <<- y
    invmatrix <<- NULL
  }
  
  get <- function () x
  setInv <- function(inverse) invmatrix <<- inverse
  getInv <- function () invmatrix
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
  
  

}


# 'cacheSolve' function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) return(inv)
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv 
}
