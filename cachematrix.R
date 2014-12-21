## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function will create and manage a special vector to cache
# an inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    
    # Initialize variable
    matrixCache <- NULL
    
    # Update matrix values and discard older
    set <- function(y) {
        x <<- y                  # Update the matrix and
        matrixCache <<- NULL     # Throw away older values from outer variables
    }
    
    # Send matrix actual values
    get <- function () x
    
    # Compute the inverse from a matrix
    # And keep this value on outer matrixCache variable
    setInverse <- function(Matrix) matrixCache <<- Matrix
    
    # Retrieve Matrix's inverse
    getInverse <- function() matrixCache
    
    list( set = set, get = get,
          setInverse = setInverse, getInverse = getInverse)
    
}


## Write a short comment describing this function
# This function will compute an inverse of a matrix
# and cache its value
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # Check if a cache already exist for this matrix
    inverseCache <- x$getInverse()    
    if(!is.null( inverseCache )){                   # If yes
        message( "getting cached data" )            # Tell the user
        return( inverseCache )                      # Send the cached matrix
    }
    
    data <- x$get()                             # Get the matrix
    inverseCache <- solve( data, ...)           # Compute the inverse
    x$setInverse( inverseCache )                # Cache the value
    inverseCache                                # Return the inverse matrix
    
}
