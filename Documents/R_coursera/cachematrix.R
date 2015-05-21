## These two functions allow to find an inverse matrix for 'x' and remember it. 
## So if one needs to find the inverse matrix several times it will be faster.

## makeCacheMatrix needs a matrix as input. The output is a list of functions: 
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse_x <- NULL
  set <- function(y){
    x <<- y
    inverse_x <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse_x <<- inverse
  getinverse <- function()    inverse_x 
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve needs the result of previous function makeCacheMatrix as input.
## Its result is a matrix that is inverse to 'x'. 
## But first of all it checks if the needed matrix has been already found. 
## If it was, then the the result is taken from cache.

cacheSolve <- function(x, ...) {
  inverse_x <- x$getinverse()
  if(!is.null(inverse_x)) {
    message("getting cached data")
    return(inverse_x)
  }
  data <-x$get()
  inverse_x <- solve(data, ...)
  x$setinverse(inverse_x)
  inverse_x
}