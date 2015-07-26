# makeCacheMatrix: This function creates a list containing the below functions 
# This function receives square matrix as input and performs below operations .

# 1. setMatrix()      assigns the value of a matrix
# 2. getMatrix()      retrieves the value of a matrix assigned
# 3. setInverse()     assigns the inverse of a matrix
# 4. getInverse()     retrieves the inverse of a matrix assigned

# Assumption : Input matrix is always invertible square matrix

makeCacheMatrix <- function(x = matrix()) {
        
        # initialize inverse matrix to NULL
        invMat <- NULL
        
        # cache the square matrix received
        setMatrix <- function(y) {
                x <<- y
                invMat  <<- NULL
        }
        
        # returns the cached square matrix
        getMatrix <- function() x
        
        # cache the inverse of the square matrix passed as argument
        setInverse <- function(inverse) invMat  <<- inverse
        
        # returns the inverse matrix from cache
        getInverse <- function() invMat 
        
        # Creates a list with functions that can be utilized via the list names.
        list(setmat = setMatrix, getmat = getMatrix, setinv = setInverse, getinv = getInverse)
}

# cacheSolve(): Input is square invertible matrix and verifies if the Inverse of the 
# given matrix is already cached. If the inverse matrix is already cached, returns the inverse matrix from cache
# If the inverse of matrix is not cached then the inverse of matrix is calculated and cached for future

cacheSolve <- function(x, ...) {
        
        # Assign the inverse matrix already cached to 'inv'. If not cached, receives NULL 
        inv <- x$getinv()
        
        # Validates the returned value so that cached data can be returned.
        if(!is.null(inv)) {
                message("Using the Cached data")
                return(inv)
        }
        
        # As the inverse of the matrix is not cached, the input matrix is assigned to 'datamat'
        datamat <- x$getmat()
        
        #Inverse of the square matrix is assigned to 'inv'
        inv <- solve(datamat)
        
        # Calculated inverse matrix is cached calling the setinv() function for further usage.
        x$setinv(inv)
        inv
}