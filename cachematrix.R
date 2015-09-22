# TODO: Add comment
# 
# Author: etoro
###############################################################################

#This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The following function calculates the inverse of the matrix. However, it first checks if
# the inverse has already been computed. If so, it gets the inverse from the cache and skips the
# computation. otherwise, it calculates the inverse of the matrix, sets the value in the cache via
# setinverse function.
# For this assignment, assume that the matrix supplied is always invertible
cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data.")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	inv
}