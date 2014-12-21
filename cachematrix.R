
## makeCacheMatrix - returns a list of function objects that provide access to
## a cached inverse matrix for a specified matrix. The access functions use the
## following names: "set", "get", "setinverse", and "getinverse"
##
## cacheSolve - returns the inverse of a "special" matrix object that was
## made using the "makeCacheMatrix" function. 

## Create a special "matrix" object which can be used to cache an inverse matrix
## for a matrix object.

makeCacheMatrix <- function(x = matrix()) {
	# the cached inverse
	i <- NULL
	
	# set the matrix that will have it's inverse cached 
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	
	# get the matrix that was used to compute the cached inverse
	get <- function() x
	
	# set the cached inverse
	setinverse <- function(inverse) i <<- inverse
	
	# get the cached inverse
	getinverse <- function() i
	
	list( set = set, get = get, setinverse = setinverse, getinverse = getinverse )
}

## Return a matrix that is the inverse of 'x', whenever the cached inverse matrix
## computation is available, that will be returned. Otherwise the inverse of the given
## matrix is solved, and the result cached and returned.

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	d <- x$get()
	i <- solve(d, ...)
	x$setinverse(i)
	i
}
