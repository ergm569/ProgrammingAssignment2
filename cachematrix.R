## makeCacheMatrix and cacheSolve are two functions that create a matrix and cache its inverse

## This function allows you to create a matrix and variables to contain the inverse
## of the matrix. It actually returns a list of functions to set and get the values
## of the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <-function(y){
    x <<- y
    inv <<- NULL
  }
  get <-function() x
  setinv <- function(inversa) inv <<- inversa
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function retrive the inverse of the matrix returned by makeCacheMatrix
## It computes and cache the inverse depending on the inverse whether the inverse
## has already been calculated

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}