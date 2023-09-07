
# This function creates a special "matrix"  to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  #initialize inverse property
  inv <- NULL
  #setting up the matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  #Method to get the matrix
  get <- function() x
  #method to setting the matrix inverse
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  #method to getting the matrix inverse
  getInverse <- function() inv
  #returning a list of methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function calculates the inverse of the special "matrix" from makeCacheMatrix.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  #getting matrix from object
  data <- x$get()
  #calculate inverse
  inv <- solve(data)
  #set inverse
  x$setInverse(inv)
  #return matrix
  inv      
}
