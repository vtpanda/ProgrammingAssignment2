## These functions provide a way to provide a way to generate and cache the inverse of a matrix for repeted uses, thereby saving time

## makeCacheMatrix provides the caching mechanism of the matrix; 
## its constructor takes a matrix argument;
## set() replaces its matrix with a new one; 
## get() returns its matrix; setinverse()
## setinverse() caches the provided inverse of the matrix
## getinverse() returns the inverse of the matrix from the cached variable

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
	get <- function() x
	
	setinverse <- function(inverse) m <<- inverse
	
	getinverse <- function() m
	
	list(set=set, get=get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve takes a makeCacheMatrix object, gets its cached inverse matrix, 
##     and, if the cached inverse matrix is not available, calculates the inverse of its matrix and sets it on the makeCacheMatrix object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        if (!is.null(m)) {
        	message("getting cached data")
        	return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
