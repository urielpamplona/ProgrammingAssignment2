


## Caching the Inverse of a Matrix:
## Inverting a matrix is not done by raising it to the power of –1, R normally 
## applies the arithmetic operators element-wise on the matrix. 
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m.inv <- NULL
        set <- function(y) {
                x <<- y
                m.inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m.inv <<- inverse
        getInverse <- function() m.inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Next function computing the inverse of a square matrix can be done with the 
## `solve` function in R. For example, if `X` is a square invertible matrix, 
## then `solve(X)` returns its inverse.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m.inv <- x$getInverse()
        if (!is.null(m.inv)) {
                message("getting cached data")
                return(m.inv)
        }
        m <- x$get()
        m.inv <- solve(m, ...)
        x$setInverse(m.inv)
        return(m.inv)
}

