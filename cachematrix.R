## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


##makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        ##m stores the cached inverse matrix
        m <- NULL 
        
        ##setting for the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ##getting for the matrix
        get <- function() x
        
        
        ##setting for the inverse
        setInverse <- function(solve) m <<- solve
        
        ##getting for the inverse
        getInverse <- function() m
        
        ##returing the matrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function


##cacheSolve computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        
        ##if the inverse matrix was already calculated, just return that matrix
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        
        ##if the inverse matrix was not calculated before, calculate it
        data <- x$get()
        m <- solve(data, ...)
        
        ##cache the inverse
        x$setInverse(m)
        
        ##return the inverse
        m
}