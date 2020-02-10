## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse

## The first function creates a special "matrix" object, which is really a list containing a function to set and get 
## the value of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The following function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse 
## has already been calculated (and the matrix has not changed), then the cacheSolve retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}


## Check if it works or not

x <- matrix(1:4, 2, 2)
x
new_x <- makeCacheMatrix(x)
cacheSolve(new_x)
