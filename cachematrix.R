## makeCacheMatrix creates 4 functions for setting and getting the 
## working matrix and its inverse


makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
                x <<- y
                m <<- NULL
        }
	get <- function() x
	setinv <- function(inv) i <<- inv
	getinv <- function() i
	list(set = set, get = get, setinv = setinv, getinv = getinv )

}


## cacheSolve checks if the inverse matrix has already been computed
## and avoid to recalculate it if the inverse matrix is already cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	i <- x$getinv()
	if(!is.null(i)) {
	message("getting cached data")
                return(i)
        }
	data <- x$get()
	i <- solve(data)
	x$setinv(i)
	i

}
