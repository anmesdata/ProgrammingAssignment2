## The two functions below aim to cache the inverse of a matrix rather than compute it repeatedly. The functions assume that the input matrix is always a valid invertible matrix

## The makeCacheMatrix function will create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	Inv <- NULL
	set <- function(y) {
		x <<- y
		Inv <<- NULL
	}
	get <- function() x
	setInv <- function(solveMatrix) Inv <<- solveMatrix
	getInv <- function() Inv
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## The cacheSolve function computes the inverse of a special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	Inv <- x$getInv()
	if(!is.null(Inv)){
		message("getting cached data")
		return(Inv)
	}
	mat <- x$get()
	Inv <- solve(mat, ...)
	x$setInv(Inv)
	Inv
}
