##  These pair of functions create a Matrix object, evaluates the inverse of matrix
##  and cache the inverse of the matrix.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  x_inverse <- NULL
  set <- function(y) {
    x <<- y
    x_inverse <<- NULL
  }
  
  get <- function() x
  setinverse<- function(inverse) x_inverse <<-inverse
  getinverse <- function() x_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix"  returned 
## by makeCacheMatrix function above but if inverse already exist  for
##this matrix object and the matrix has not changed,the inverse is retrieved 
## from cache by this function

##Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  
  x_inverse <- x$getinverse()
  if (!is.null(x_inverse)) {
    message("getting already cached inverse ")
    return(x_inverse)
  } 
  data <- x$get()
  x_inverse <- solve(data)
  x$setinverse(x_inverse)
  x_inverse
  
  
}