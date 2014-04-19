## Pair of functions that compute and cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse 
## (however, as well as any other value provided by setcached())

makeCacheMatrix <- function(x = matrix()) {
        c <- NULL
        set <- function(y) {
                x <<- y
                ## since we set new value, we also have to reset the cached value
                ## in case it was already set
                c <<- NULL
        }
        get <- function() x
        setcached <- function(cached) c <<- cached
        getcached <- function() c
        list(set = set, get = get,
             setcached = setcached,
             getcached = getcached)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated, then the inverse retrieved from the cache.

cacheSolve <- function(x, ...) {
        
        s <- x$getcached()
        
        if(!is.null(s)) {
                message("getting cached data")
                ## we already have cashed value, so we just return it
                return(s)
        }
        
        data <- x$get()
        s <- solve(data, ...)
        x$setcached(s)
        
        ## Return a matrix that is the inverse of 'x'
        s
}
