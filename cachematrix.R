## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.
## This functions create a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        ## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
        inv <- NULL
        set <- function(y) {
                x <<- y                                                                             ## set the value of the vector
                inv <<- NULL
        }
        get <- function() x                                                               ## get the value of the vector
        setInverse <- function(inverse) inv <<- inverse         ## set the value of the inverse
        getInverse <- function() inv                                            ## get the value of the inverse
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This second function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {                                                                 ## Checks if the inverse has already been calculated
                message("getting cached data")
                return(inv)                                                                  ## If so, it gets the mean from the cache and skips the computation
        }
        mat <- x$get()
        inv <- solve(mat, ...)                                                         ## Otherwise, it calculates the mean of the data and,
        x$setInverse(inv)                                                             ## sets the value of the mean in the cache via the setinverse function
        inv
}