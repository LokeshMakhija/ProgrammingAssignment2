## makeCacheMatrix creates a special matrix, given a matrix as an input. It has special functions which 
## can cache the inverse of the original matrix through setInv function and returns the inverse value 
## when requested through getInv function.  

## eg. to run 
## x <- makeCacheMatrix(matrix(rnorm(25),5,5))
## cacheSolve(x)
## cacheSolve(x)


makeCacheMatrix <- function(x = matrix()) {
	
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInv <- function(invr) inv <<- invr
	getInv <- function() inv
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve takes a matrix formed by makeCacheMatrix function. It checks if the inverse of the matrix 
## is already in cache. If yes, it returns the value, else calculates the inverse, stores it in cache and
## then returns the value.

cacheSolve <- function(x, ...) {
        i <- x$getInv()
	if (!is.null(i)){
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	invr <- solve(data)
	x$setInv(invr)
	invr
}
