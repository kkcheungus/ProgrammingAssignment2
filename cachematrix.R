## The first function, `makeCacheMatrix` creates a special "Matrix", 
## which is in fact a list containing the following functions to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse in "cache"
## 4.  get the value of the inverse from "cache"

makeCacheMatrix <- function(x = matrix()) {
  cachedInverseMatrix <- NULL
  setSquareMatrix <- function(y) {
    x <<- y
    cachedInverseMatrix <<- NULL
  }
  getSquareMatrix <- function() x
  setCachedInverseMatrix <- function(inverse) cachedInverseMatrix <<- inverse
  getCachedInverseMatrix <- function() cachedInverseMatrix
  list(setSquareMatrix = setSquareMatrix, 
       getSquareMatrix = getSquareMatrix,
       setCachedInverseMatrix = setCachedInverseMatrix,
       getCachedInverseMatrix = getCachedInverseMatrix) 
}


## The second function solve the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been solved If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it solve the inverse of
## the matrix and sets the value of the inverse in the cache via the `setInverseMatrix`
## function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getCachedInverseMatrix()
  if(!is.null(inverseMatrix)) {
    message("getting cached inverse matrix")
    return(inverseMatrix)
  }
  squareMatrix <- x$getSquareMatrix()
  inverseMatrix <- solve(squareMatrix)
  x$setCachedInverseMatrix(inverseMatrix)
  inverseMatrix
}