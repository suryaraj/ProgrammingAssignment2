# The function makeCacheMatrix takes a matrix as an argument and creates the matrix;
# The function cacheSolve takes a matrix as an argument and computes its inverse and if it has already the inverse 
  # then it returns the cache value without computing it again.

## This function initializes the object i as NULL, and defines four function namely, set, get, setInverse, getInverse
   ### set function copies the matrix n, that it takes as an argument and initializes the  object i to NULL
   ### get function returns the current matrix
   ### setInverse function receives the inverse of the function and copies it to the object i
   ### getInverse function returns the inverse of the current matrix 

makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  set <- function(n){
    m <<- n
    i <<- NULL
  }
  get <- function() m
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function cache solve initializes the object i with the inverse value 
## and returns the inverse value of the current matrix
   ### Computes the inverse of the current matrix
   ### If the current matrix has the inverse(calculated early), then it returns that matrix from the cache displaying
       #### the message "getting cached data".


cacheSolve <- function(m, ...){
  i <- m$getInverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  matrix <- m$get()
  i <- solve(matrix, ...)
  m$setInverse(i)
  i
}
