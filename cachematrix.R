## Put comments here that give an overall description of what your
## functions do:
## The function makeCacheMatrix will generate a square invertible matrix using a numeric
## input. This numeric input is the dimension of the square matrix. For example: makeCacheMatrix (4)
## will create a 4x4 matrix with random numbers. In addition this function will make a list
## of other functions that will store the inverse of the created matrix calculated by the
## the second function cacheSolve. Once calculated the inverse of
## the original matrix will be stored and will be retrieved by cacheSolve without the need
## to recalculate.

## Write a short comment describing this function
## This function will create a square invertile matrix using the numberic input, which
## specifies the matrix dimension. For instance, makeCacheMatrix (4) creates a 4x4 matrix

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  get_m <- function () matrix(rnorm(x^2),x,x)
  setinv_m <- function(inv) m <<- inv
  getinv_m <- function() m
  list(get_m = get_m,
       setinv_m = setinv_m,
       getinv_m = getinv_m)

}


## Write a short comment describing this function
## This function will calculate the inverse of the matrix generated in makeCacheMatrix if it has never been 
## calculated. Once calculated for the first time, the results (the inverse matrix) will be cached in makeCacheMatrix
## Then, next time cacheSolve is run, it will not recalculate the inverse of the original matrix, but will 
## retrieve the results in the cache. Therefore, saving on computation time.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv_m()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  inv <- solve(x$get_m())
  x$setinv_m(inv)
  inv
}
