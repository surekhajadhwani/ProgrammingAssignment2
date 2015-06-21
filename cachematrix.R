## Matrix inversion is usually a costly computation and there may be some benefit to caching the
## inverse of a matrix rather than compute it repeatedly. This assignment is to write a pair of 
## functions that cache the inverse of a matrix.

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## initialize the value to NULL
    m <- NULL
  
    ## sets the value
    set <- function(y){
        x <<- y
        ## update the value of inverse matrix
        m <<- NULL
    }

    ## gets the value
    get <- function() x

    ## set the inverse of matrix
    setmatrix <- function(solve) m <<- solve
    ## get the invers of matrix
    getmatrix <- function() m
    
    list(set = set, get = get,
    setmatrix = setmatrix,
    getmatrix = getmatrix)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getmatrix()
    ## check if inverse exists in cache, if yes, get the inverse from cache
    if(!is.null(m)){
        message("getting cached inverse of matrix")
        return(m)
    }
    ## else compute the inverse and add to cache
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
}