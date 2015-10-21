## Program assignement #2
# Intial date: October 21, 2015
# Author : JD

# This code contains two functions for caching the inverse of a matrix

# Example of use:
# > a<-matrix(1:4,2,2)
# > a
#       [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > u<-makeCacheMatrix(a)
# > cacheSolve(u)
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(u)
# getting cached data
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5


## This function creates a special "matrix" object that can cache its inverse.
# It creates a special "matrix", which is really a list containing a function to
#    set the value of the matrix
#    get the value of the matrix
#    set the value of the inverse of the matrix
#    get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) 
    {
    x <<- y
    inv <<- NULL
    }
  get <- function() x
  setinv <-function(solve) inv <<- solve
  getinv <-function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
   }

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed)
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) 
  {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
