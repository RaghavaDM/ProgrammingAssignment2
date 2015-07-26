# makeCacheMatrix: This function creates a list containing the below functions 
# 1. set() - Assigns the value of a matrix
# 2. get() - Retrieves the value of a matrix assigned
# 3. setInv() - Assigns the inverse of a matrix
# 4. getInv() - Retrieves the inverse of a matrix assigned
# This function receives square inversible matrix as input and performs one of the above 
# actions based on the function called.
# Assumption is that the received matrix is inversible square matrix

makeCacheMatrix <- function(x = matrix()) {
  # initialize the m value to NULL
  m <- NULL
  
  # Cache the square matrix received
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # Returns the cached square matrix
  get <- function() x
  
  # cache the inverse of the square matrix passed a actual argument
  
  setInv <- function(solve) m <<- solve
  # Returns the cached inverse matrix
  getInv <- function() m
  
  # Create a list with all the functions so that they can be used via list names.
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

# cacheSolve(): This function receives a square matrix and verifies if the Inverse of the 
# given matrix is already cached. If the cached inverse matrix is already available then it is returned.
# If the cache data is not found then the inverse of the matrix is calculated, 
# also caching it further usage

cacheSolve <- function(x, ...) {
  # Assign the inverse matrix already cached to 'inv'. If not cached, receives NULL 
  m <- x$getInv()
  # Validates the returned value so that cached data can be returned
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # As the inverse of the matrix is not cached, the input matrix is assigned to 'data'
  data <- x$get()
  #Inverse of the square matrix is assigned to 'm'
  m <- solve(data, ...)
  # Calculated inverse matrix is cached calling the setInv() function for further usage.
  x$setInv(m)
  m
}