## This function creates a matrix that can cache its inverse. 
##It uses a list to store the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  	inv <- NULL  

  	set <- function(y) {
    		x <<- y
    		inv <<- NULL  
  	}
  
  	get <- function() x
 
 	setInverse <- function(inverse) inv <<- inverse

  	getInverse <- function() inv

  	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix. 
##It retrieves the inverse from the cache if it is already computed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  	inv <- x$getInverse()
  	if (!is.null(inv)) {
    		message("Getting cached data")
    	return(inv)  
  	}
  
  	mat <- x$get()  
 	 inv <- solve(mat, ...)  
  	x$setInverse(inv)  
  	inv
}
