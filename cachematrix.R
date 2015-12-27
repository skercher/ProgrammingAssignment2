## Author @skercher
## Caching the Mean of a Vector
## Matrix inversion is usually a costly computation and there may be some
## Benefit to caching the inverse of a matrix rather than compute it repeatedly

## This function creates a special "matrix" object that can cache its inverse.
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


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	    ## Return a matrix that is the inverse of 'x'
	           inversion <- x$getInverse()
	           if (!is.null(inversion)) {
	                   message("getting the cached data.")
	                   return(inversion)
	           }
	           matr <- x$get()
	           inversion <- solve(matr, ...)
	           x$setInverse(inversion)
	           inversion
}
