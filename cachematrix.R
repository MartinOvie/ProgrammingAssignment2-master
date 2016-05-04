## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
  x_inverse <- NULL
  set <- function(y) 
  {
    x <<- y
    x_inverse <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) x_inverse <<-inverse
  getinverse <- function() x_inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
  x_inverse <- x$getinverse()
  if (!is.null(x_inverse)) 
  {
    message("getting cached inverse matrix")
    return(x_inverse)
  } 
  else 
  {
    x_inverse <- solve(x$get())
    x$setinverse(x_inverse)
    return(x_inverse)
  }
}
