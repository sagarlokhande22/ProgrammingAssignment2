## OVERALL DESCRIPTION
## Following is a pair of functions that cache the inverse of a matrix. 
## For matrices that are very large in size, it might be a waste of computational power to 
## calculate inverse matrix if the contents of original matrix is not changing frequently. 
## So it might make sense to cache the value of the inverse so that when we need it again, 
## it can be looked up in the cache rather than recomputed.

## makeCacheMatrix DESCRIPTION
## The first function, makeCacheMatrix creates a vector containing 4 functions to
#1 set the matrix - here referred as setmatrix()
#2 get the matrix - here referred as getmatrix()
#3 set the inverse - here referred as setinverse()
#4 get the inverse - here referred as getinverse()


makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  setmatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(abc) inv <<- abc
  getinverse <- function() inv
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve DESCRIPTION
## The following function calculates the inverse of the matrix created with the above 
## function. However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse
## in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse data")
    return(inv)
  }
  data <- x$getmatrix()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
