## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). ## my assignment is to write a pair of functions that cache the inverse of a matrix. Please see them below:

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## Computing the inverse of a square matrix can be done with the solve function in R. 

## Its assumed that the matrix given is always invertible

## This function makeCacheMatrix creates a special "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

} ## end of makeCasheMatrix


## This function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## Computing the inverse of a square matrix can be done with the solve function in R.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting inversed data from cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m # Return a matrix that is the inverse of 'x'
} ## end of cacheSolve
