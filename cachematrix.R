## Caching the Inverse of a Matrix
## Two functions: makeCacheMatrix and cacheSolve
## makeCacheMatrix() - This function creates a special "matrix" object that can cache its inverse
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

## function to create a special matrix which is list of functions that does the following
## set the value
## Get the value
## set the value of inverse
## get the value of inverse
makeCacheMatrix <- function(x = matrix()) {
    ## set the value
    m <- NULL
    set <- function(y) {
                        x <<- y
                        m <<- NULL
                        }
    ## get the value
    get <- function() x
    ## set the value of inverse
    setInverse <- function(inverse) m <<-inverse
    ## get the value of inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of the matrix returned by makeCacheMatrix(), If the inverse has
## already been calculated, then it retrieves it from the cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    ##return cached inverse
    if ( ! is.null(m)) {
       print("getting cached data")
       return(m)
       }
    ##calculate inverse
    m <- solve(x$get())
    x$setInverse(m)
    m
}
