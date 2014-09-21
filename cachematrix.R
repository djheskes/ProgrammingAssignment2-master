## Put comments here that give an overall description of what your
## functions do

## function allows to save the inv of a matrix x. the function caches the matrix itself and its inverse. 
## 1. set (or load) the matrix x. 2. get() matrix x 3. set inverse of matrix x 4. get inverse of matrix x.

makeCacheMatrix <- function(x = matrix()) {

	  m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve(solve)
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## returns the inverse of x. IF the inverse is NOT stored it is calculated and cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'


      v <- x$getinv()
	if (!is.null(v)) {
		message("using cache")
		return(x$getinv())
	} else {
	  data <- x$get()
        v <- solve(data)
        x$setinv(data)
        v 
	}

}
