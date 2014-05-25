## Touhidur (May 25, 2014)

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## a. set the value of the matrix
## b. get the value of the matrix
## c. set the value of the solve
## d. get the value of the solve

## The second function, cacheSolve compute the inverse of the special "matrix" 
## return by makeCacheMatrix. 
## However, it first checks to see if the inverse has already been computed.
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it computes the inverse of the matrix and 
## sets the value of the inverse in the cache via the setmatrix function.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}
