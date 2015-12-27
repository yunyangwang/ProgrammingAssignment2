## In order to diminish the costly effects of repeated computations of
## the inverse of matrices, these two functions have been created to
## store a matrix and cache its inverse.

## This first function creates a special "matrix" that can cache its
## inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) i <<- inverse
	getinv <- function() i
	list(set = set, get = get,
	setinv = setinv,
	getinv = getinv)
}


## This second function computes the inverse of the "matrix" created 
## by the makeCacheMatrix
## If the inverse has already been calculated, and the matrix has not
## changed, then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
	i <- x$getinv()
	if(!is.null(i)) {
			message("getting cached data")
			return(i)
	}
	matrx <- x$get()
	i <- solve(matrx, ...)
	x$setinv(i)
	i
}
