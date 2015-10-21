## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
# this function returns inverse matrix it either:
#	* returns cached value if exists, or
#	* computes inverse value, stores it in cache and then returns it
#	matrix is assumed to be solvable

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	# get the cached value first using getinverse
	m <- x$getinverse()
	# if cached value exists print info and return it
	if(!is.null(m)) {
		message("getting cached inverse matrix")
		return(m)
	}
	# if not cached, compute inverse matrix
	data <- x$get()
	m <- solve(data, ...)
	# cache the inverse matrix
	x$setinverse(m)
	# return inverse matrix
	m
}
