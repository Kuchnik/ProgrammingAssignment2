## The program creates a matrix object and returns an inverse of the matrix;
## if the inverse was already calculated once, the inverse woud be retrieved from the cache,
## otherwise the inverse is calculated

## This function creates a matrix object

makeCacheMatrix <- function(x = matrix()) {
    inverse_x <- NULL
    set <- function(y) {
        x <<- y
        inverse_x <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) inverse_x <<-inverse
    getinverse <- function() inverse_x
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)

}


## This function calculates and returns the inverse of the matrix
## or takes the result from the cache if it was already calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_x <- x$getinverse()
        if (!is.null(inverse_x)) {
            message("getting cached inverse matrix")
            return(inverse_x)
        }
            inverse_x <- solve(x$get())
            x$setinverse(inverse_x)
            return(inverse_x)
        
}
