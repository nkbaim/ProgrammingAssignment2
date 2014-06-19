## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  rm <- NULL
  set <- function(y) {
    x <<- y
    rm <<- NULL
  }
  get <- function() x
  setReverseMatrix <- function(revesematrix) rm <<- revesematrix
  getReverseMatrix <- function() rm
  list(set = set, get = get,
       setReverseMatrix = setReverseMatrix,
       getReverseMatrix = getReverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  rm <- x$getReverseMatrix()
  if(!is.null(rm)) {
    message("getting cached data")
    return(rm)
  }
  data <- x$get()
  rm <- solve(data, ...)
  x$setReverseMatrix(rm)
  rm
}
