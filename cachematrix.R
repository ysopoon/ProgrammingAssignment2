## The following pair of functions cache the inverse of a matrix

## The function, makeCacheMatrix creates a spectial "matrix" object that 
## can cache its inverse. 
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m 
    list(set = set, get = get, 
         setsolve = setsolve, 
         getsolve = getsolve)
}


## The function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
