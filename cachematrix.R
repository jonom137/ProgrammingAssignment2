## These functions calculate, store and retrieve the inverse of a matrix 
## provided to them.

## The makeCacheMatrix function receives a matrix and provides functionality to 
## retrieve this matrix, set a cached inverse of this matrix and retrieve
## this cached matrix

makeCacheMatrix <- function(x = matrix()) {
        m = matrix
        set <- function (y = matrix) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, 
             getinverse = getinverse,
             setinverse = setinverse)
}


## The cacheSolve function checks to see if a matrix inverse has been cached.
## If so it retrieves and prints it. Otherwise it calculates the matrix inverse
## caches it for later retrieval and prints the result.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting chached data")
                return (m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
          
}
