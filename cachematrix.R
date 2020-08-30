## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL ## Initialize the inverse property
  ## Method to set the matrix
  set <- function(matrix){
    x <<- matrix
    i <<- NULL 
  }
  ## Method to get the matrix
  get <- function(){
    m ## returns the matrix
  }
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse){
    i <<- inverse  
  }
  ## Method to get the inverse of the matrix 
  getInverse <- function(){
    i
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Just return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
}
