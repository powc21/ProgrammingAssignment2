## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly 
## This code is a pair of functions that cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set_matrix <- function(y){
    x <<- y
    inv_matrix <<- NULL
  }
  
  get_matrix <- function()x
  set_inv <- function(inverse) inv_matrix <<- inverse
  get_inv <- function()inv_matrix
  list(set_matrix = set_matrix, get_matrix = get_matrix, set_inv = set_inv, get_inv = get_inv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv_matrix <- x$get_inv()
  
  if (!is.null(inv_matrix)) {
    message("loading cache")
    return(inv_matrix)
  }
  
  m <- x$get_matrix()
  inv_matrix <- solve(m,...)
  x$set_inv(inv_matrix)
  
  inv_matrix
}
