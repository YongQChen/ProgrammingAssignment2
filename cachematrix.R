## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly 
## The following pair of functions can be used in combination to cache the 
## inverse of a matrix.  The makeCacheMatrix function use the <<- operator
## to assign a value to an object in an environment that is different from
## the current environment

 
## This function creates a special "matrix" object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) { 
    ## x is a square invertible matrix
    ## Returns a list containing functions
    ## 1. set    - set the value of the matrix
    ## 2. get    - get the value of the matrix
    ## 3. setinv - set the value of the inverse of the matrix
    ## 4. getinv - get the value of the inverse of the matrix

    inv <- NULL
    set <- function(y) {
            x <<- y
            inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
} 

 
 
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
 
cacheSolve <- function(x, ...) { 
    ## x is the special matrix object created by makeCacheMatrix
    ## Return a matrix that is the inverse of 'x' 
    
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("get cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
} 
