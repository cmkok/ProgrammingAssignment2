## Caching the Inverse of a Matrix
## 9/18/2014

## Creates a list of functions to set and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  
  #Return a list of functions
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Check if the matrix is identical to cache matrix, retrieve the result from cache
## else compute and store into variable "m".

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' from cache
  m <- x$getsolve()
  if( !is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # Calculate the inverse with solve function and store the result to m
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

#test the functions with a 2x2 matrix
simple_matrix <- matrix(
  c(4, 2, 7, 6), 
  nrow=2, 
  ncol=2)

listed_matrix <- makeCacheMatrix(simple_matrix)
cacheSolve(listed_matrix)
#Data will retrieve from cache
cacheSolve(listed_matrix)



