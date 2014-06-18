## The overall purpose of the functions is to compute the inverse of
## an invertible matrix. To optimally use the resources, the inverse is first 
## checked for in the cache (inverse of the same matrix may have been computed
## earlier and cached and need not be recomputed) and if present, is returned
## as the result, else the inverse is computed, cached and returned.


## The makeCacheMatrix function creates a special matrix object which
## essentially is a list of functions which enable the matrix (passed as an 
## argument to the function) to cache its inverse. The list of functions
## include one to set the value of the matrix, one to get the value of the
## matrix, one to set the value of the inverse and one to get the inverse
## value.  

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) inverse <<- inv
	getinverse <- function() inverse
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

## The cacheSolve function returns the inverse of the special matrix object
##(passed as an argument to it) created using the makeCacheMatrix function. It
## first checks if the inverse has been cached, if it has then it returns this
## cached value, otherwise it computes the inverse, caches it and returns it. 

cacheSolve <- function(x, ...) {
	inverse <- x$getinverse()
	if(!is.null(inverse)) {
	message("Getting cached data")
	}
	else {
		data <- x$get()
    		inverse <- solve(data)
  		x$setinverse(inverse)
	}
	inverse        
}