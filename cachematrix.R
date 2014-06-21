## The two functions makeCacheMatrix and cache solve in conjugation stores the matrix and calculate the inverse of the matrix


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
		inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(mInverse) inverse <<- mInverse
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## The function can calculate the inverse of the matrix only if it is a square matrix with non zero determinant.
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("Getting cached inversed matrix data...")
                return(inverse)
        }
		message("Calculating inversed matrix data...")
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
