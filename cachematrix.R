## makeCacheMatrix/cacheSolve create a pair of function that allows the caching
## of calculated inverses of matrices.

## makeCacheMatrix provides a list of functions that are required to store and
## fetch calculated inverses of matrices

makeCacheMatrix <- function(x = matrix()) {
  
  # constructor of the function: inv is always initialized by NULL
  inv <- NULL
  
  # set x in the underlying vector to y and reset the unerlying inv
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # return data stored in x
  get <- function() {x}
  
  # sets the inverse in the underlying vector
  setinv <- function(inv) {inv <<- inv}
  getinv <- function() {inv}
  
  #this returns the list of functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  #sets the local inverse to the one stored in the underlying vector
  inv <- x$getinv()
  
  # if that data ist not NULL then it can be returned directly from cache
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # if not, compute the inverse using solve function and return it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
