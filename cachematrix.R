## Programming assignment 2: Caching the Inverse of a Matrix
## Calculate inverse of a matrix, with caching calculated result.
##
## This file contains 2 companion functions:
## - makeCacheMatrix: get function vector used by cacheSolve.
## - cacheSolve: get inverse of a matrix. use with makeCacheMatrix.
## 
## Examples
##   x <- matrix(c(1,2,3,4), nrow=2, ncol=2)
##   m <- makeCacheMatrix(x)
##   cacheSolve(m) # first call
##   cacheSolve(m) # second call, "getting cached data"


# create function vector that get/set a matrix, 
# and cached inverse of the matrix.
makeCacheMatrix <- function(a = matrix()) {
  ia <- NULL
  setMatrix <- function(b) {
    a <<- b
    ia <<- NULL
  }
  getMatrix <- function() a
  setInverseMatrix <- function(ib) ia <<- ib
  getInverseMatrix <- function() ia
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix, 
       getInverseMatrix = getInverseMatrix)
}


# Return a matrix that is the inverse of 'm'
# This function assumes 'm' is invertible.
cacheSolve <- function(m, ...) {
  cache_ia <- m$getInverseMatrix()
  if(!is.null(cache_ia)){
    message("getting cached data")
    return(cache_ia)
  }
  a <- m$getMatrix()
  ia <- solve(a)
  m$setInverseMatrix(ia)
  ia
}
