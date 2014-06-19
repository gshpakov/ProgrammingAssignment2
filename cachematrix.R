## This code implements special "matrix" object that represents the provided
## true matrix object and also its inversed version.
## The inversed version is calculated and cached only on the first request

## This function creates and initializes our object
makeCacheMatrix <- function(x = matrix())
{
  x.inverse <- NULL
  set <- function(y = matrix()) {
    x <<- y
    x.inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) x.inverse <<- inverse
  getinverse <- function() x.inverse

  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function uses previously created object to calculate and cache
## the inversed matrix
cacheSolve <- function(x, ...)
{
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  message("calculating the data...")
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)

  ## Return a matrix that is the inverse of 'x'
  inv
}
