## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function take in a Matrix as an argument and return a
# list of 4 functions
# 1. set(): Set the matrix
# 2. get(): Get the matrix
# 3. setinvert(): Set the invert of the matrix
# 4. getinvert(): Get the invert of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvert<- function(invert) m <<- invert
  getinvert <- function() m
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}


## Write a short comment describing this function
# This function take in a funcion as an argument and calculate
# the invert of the matrix.
# It first check if the inverted matrix have been calculated
# by useing the getinvert() function and will return the inverted
# matrix.  If not, it will call the solve() function to create
# and return the inverted matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinvert()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvert(m)
  m    
}
