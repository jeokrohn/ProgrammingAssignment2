# Functions to help to cache results of time consuming operations. In this case
# the result of matrix inversion will be cached so that for multiple requests to
# return the inverse of the same matrix the actual matrix inversion is only done
# once and subsequent requests are servers from a cache

# makeCacheMatrix creates a list of methods to get and set the value of the 
# matrix and the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    cinv <- NULL
    # set a new cached matrix
    set <- function(y) {
        # cached matrix saved in x
        x <<- y
        # reset the cached inverse
        cinv <<- NULL
    }
    
    # return the cached matrix
    get <- function() x
    
    # set a new cached inverse
    setinv <- function(inv) cinv <<- inv
    
    # get the current cached inverse
    getinv <- function() cinv
    
    # return a list of get/set methods for matrix and inverse
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


# Calculate the inverse of a given matrix. To access the matrix the functions
# uses a list created by the above function to access the matrix to invert and
# to cache the result. If the inverse has been calculated before then the 
# function just returns the already calculated inverse.
cacheSolve <- function(x, ...) {
    # get the cached inverse
    inv <- x$getinv()
    if(!is.null(inv)) {
        # just return the existing cached inverse
        message("getting cached data")
        return(inv)
    }
    # if the matrix hasn't been inverted before do it now:
    # get the current matrix
    mat <- x$get()
    # invert it
    inv <- solve (mat)
    # cache the inverted matrix
    x$setinv(inv)
    # return the inverted matrix
    inv
}
