## cachematrix.R
## Author: Alex Mazurkiewicz
## Created: 14 Apr 2016
## Last revised: 14 Apr 2016

## The following pair of functions can be used to cache the inverse of a matrix.

## makeCacheMatrix creates a matrix-object that can cache its inverse via a function.

makeCacheMatrix <- function(x = matrix()) {

    # Create a matrix-object that can cache its inverse using 4 functions
    
    # Initialization of inverse matrix
    inv <- NULL
    
    # Function to (re)set the matrix, causing the inverse to revert to NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Function to get the matrix
    get <- function() x
    
    # Function to set the inverse
    setInv <- function(inverse) inv <<- inverse
    
    # Function to get the inverse
    getInv <- function() inv
    
    # Return matrix-object as a list
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
    
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache. Otherwise, the inverse
## will be computed and stored in the cache.

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    
    ## Get the inverse matrix, if it was already calculated and stored in the cache
    inv <- x$getInv()
    
    ## Check to see if the inverse actually exists (ie NOT null). If yes, return it
    if (!is.null(inv)) {
        message("Getting cached inverse...")
        return(inv)
    }

    ## If no, calculate the inverse, cache it, and return it
    else {
        message("Calculating inverse matrix...")
        matrix <- x$get()
        inv <- solve(matrix)
        x$setInv(inv)
        return(inv)
    }
}


## Testing
# x = matrix(c(2,4,5,5,2,1,1,2,2), nrow=3,ncol=3)
# m <- makeCacheMatrix(x)
# m.i <- cacheSolve(m)
