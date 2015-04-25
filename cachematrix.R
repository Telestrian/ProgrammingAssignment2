
# This function contains a list of functions that can be called by the second function. 
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # Sets the contents of the matrix(x) to those of matrix y. The contents of matrix y are those passed into x$set() 
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    # the get function prints the current values of matrix x when it is called.
    get <- function() x
    # sets the solve function to the function in use
    setinverse <- function(solve) m <<- solve
    # applies the function 
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }




# stores the results of get/getinverse function applied to matrix x to variable m to cache it.

cacheSolve <- function(x, ...) {
# stores result of getinverse function and returns it
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #stores result of get function to data, t.
  data <- x$get()
  #Stores result of applying solve function to data in m and prints it.
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

