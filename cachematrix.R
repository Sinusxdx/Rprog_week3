## Put comments here that give an overall description of what your
## functions do

## Returns a list of functions associated to a matrix

makeCacheMatrix <- function(x = matrix())
{
    inv <- NULL   ## reset inv; inv stands for inverse
    set <- function(y) {   ## set, get, setinverse and getinverse are functions analogos to the example with cachemean
        x <<- y
        inv <<- NULL
    }
    get <- function() {x}
    setinverse <- function(inverse) {inv <<- inverse}
    getinverse <- function() {inv}
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
