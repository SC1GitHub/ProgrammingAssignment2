## makeCacheMatrix function creates a special "matrix" object that can cache its inverse
## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse 
## has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from 
## the cache.

## makeCacheMatrix function will initialize the inverse of the matrix first. Then, it will set the matrix, get the matrix,
## set the inverse & finally get the inverse.
makeCacheMatrix <- function(x = matrix()) {
        # initialize the inverse (i)        
        i <- NULL
        # 1. set the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        # 2. get the matrix
        get <- function() x
        # 3. set the inverse
        setinverse <- function(inverse) i <<- inverse
        # 4. get the inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
####

## cacheSolve function will compute the inverse of the special "matrix" returned by the makeCacheMatrix function above
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}