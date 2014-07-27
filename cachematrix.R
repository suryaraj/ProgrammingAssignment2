# The function makeCacheMatrix takes a matrix as an argument and creates the matrix;
# The function cacheSolve takes a matrix as an argument and computes its inverse and if it has already the inverse 
## then it returns the cache value without computing it again.

## This function initializes the object inverse as NULL, and defines four function namely, set, get, setInverse, getInverse
### set function copies the matrix new_matrix, that it takes as an argument and initializes the  object inverse to NULL
### get function returns the current matrix
### setInverse function receives the inverse of the function and copies it to the object inverse
### getInverse function returns the inverse of the current matrix 

makeCacheMatrix <- function(current_matrix = matrix()) {
  inverse <- NULL
  set <- function(new_matrix){
    current_matrix <<- new_matrix
    inverse <<- NULL
  }
  get <- function() current_matrix
  setInverse <- function(newinverse) inverse <<- newinverse
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function cache solve initializes the object inverse with the inverse value 
## and returns the inverse value of the current matrix
### Computes the inverse of the current matrix
### If the current matrix has the inverse(calculated early), then it returns that matrix from the cache displaying
#### the message "getting cached data".


cacheSolve <- function(current_matrix, ...){
  inverse <- current_matrix$getInverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  matrix <- current_matrix$get()
  inverse <- solve(matrix, ...)
  current_matrix$setInverse(inverse)
  inverse
}
