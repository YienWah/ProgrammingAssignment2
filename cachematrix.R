## Provides the capability to cache an inversed marix that can be reused for a custom solve function

## Creates a special matrix object that can cache an inversed matrix and return the cache.

makeCacheMatrix <- function(x = matrix()) {

        s <- NULL
        m <- NULL
        
        ## Flushes the cache when a new matrix is set through the set function
        setMatrix <- function(y) { 
                x <<- y
        } 
        
        ## Allows one to grab the matrix
        getMatrix <- function() x 
        
        ## Caches the matrix
        setCMatrix <- function(matrix) m <<- matrix
        
        ## Allows one to grab the cached matrix
        getCMatrix <- function() m
        
        ## Caches the inversed matrix
        setInvMatrix <- function(solve) s <<- solve
        
        ## Allows one to grab the inversed matrix that may have been cached
        getInvMatrix <- function() s
        
        ## Returns a list of functions
        list(setMatrix = setMatrix, 
             getMatrix = getMatrix,
             setCMatrix = setCMatrix,
             getCMatrix = getCMatrix,
             setInvMatrix = setInvMatrix,
             getInvMatrix = getInvMatrix)

}


## Computes an inverse of the provided matrix when the inversed matrix has not been cached
## or when the matrix changed

cacheSolve <- function(x, ...) {

        ## Gets the possibly cached inversed matrix
      	s <- x$getInvMatrix()
        
        ## Checks if the inversed matrix was previously cached and if the matrix is still the same. If yes, then returns the cache.
        if(!is.null(s) && identical(x$getMatrix(),x$getCMatrix())) {
                message("Getting Cached Data")
                return(s)
        }
        
        ## Inverses the matrix
        data <- x$getMatrix()
        s <- solve(data, ...)
        
        ## Caches the matrix and inversed matrix
        x$setCMatrix(data)
        x$setInvMatrix(s)
        s

}

