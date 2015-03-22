# Functions to allow for caching of the results of inverting a supplied matrix.
# Should be called in the order, makeCacheMatrix() followed by cacheSolve()
# on the resulting vector.
#
# This may be used in loops and other constructs to avoid expensive
# recalculation.


# Creates a vector that contains a contains a set of functions to
# Get and set the matrix value and get and set the inverse.
#
# Args:
#   x: The matrix that will have its inverse cached.
#
# Returns:
#   The vector that holds get and set functions for the value and inverse,
#   along with the values.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


#  Solve for the inverse of the matrix stored in the vector x, which contains
#  the results of makeMatrixCache().
#  If the matrix has already had its inverse calculated, it will return a
#  cached value, instead of recalculating.
#
#  Args:
#    x: The vector resulting from makeMatrixCache() of the value matrix.
#
#  Returns:
#     The inverse of the matrix stored in x.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("Getting cached inverse.")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}