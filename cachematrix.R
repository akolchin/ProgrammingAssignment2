## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function 

makeCacheMatrix <- function(x = matrix()) {
        c <- NULL
        set <- function(y) {
                x <<- y
                c <<- NULL
        }
        get <- function() x
        setcached <- function(cached) c <<- cached
        getcached <- function() c
        list(set = set, get = get,
             setcached = setcached,
             getcached = getcached)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
        c <- x$getcached()
        
        if(!is.null(c)) {
                message("getting cached data")
                return(c)
        }
        
        data <- x$get()
        c <- solve(data, ...)
        x$setcached(c)
        
        ## Return a matrix that is the inverse of 'x'
        c
}
