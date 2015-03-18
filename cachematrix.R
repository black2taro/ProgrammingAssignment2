## cachematrix.R
#############################################################
## Programming Assignment 2 of R Programming @ Coursera
##
## Purpose: Write functions to 
##   (1) create a matrix that can cache its inverse
##   (2) get an inverse matrix by calculation only if
##       it is not cached
##
## Created: 03/18/2015 
#############################################################

## Make a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) inv <<- inverse
    get_inverse <- function() inv
    list(set = set, get = get,
         set_inverse = set_inverse, get_inverse = get_inverse)
}

## Given a matrix created by makeCacheMatrix(),
## return the inverse of the matrix
cacheSolve <- function(x, ...) {
    inv <- x$get_inverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$set_inverse(inv)
    inv
}
