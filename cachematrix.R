## These functions calculate, store and retrieve the inverse of a matrix 
## provided to them.

## The makeCacheMatrix function receives a matrix and provides functionality to 
## retrieve this matrix, set a cached inverse of this matrix and retrieve
## this cached matrix

makeCacheMatrix <- function(x = matrix()) {
        ## Create an initital blank matrix
        m = matrix
        ## Sets a new source matrix passed to the function
        set <- function (y = matrix) {
                x <<- y
                m <<- NULL
        }
        ## Provides functionality to retrieve the current source matrix
        get <- function() x
        ## Sets the matrix inverse into cache
        setinverse <- function(inverse) m <<- inverse
        ## Provides functionality to retreive the matrix inverse
        getinverse <- function() m
        ## Provides the list of this functions subfunctions
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
        ## Checks to see if the retrieved matrix is not blank and prints
        ## it if this is the case 
        if(!is.null(m)) {
                message("getting chached data")
                return (m)
        }
        ## Otherwise the source matrix is retrieved, the inverse is caluclated
        ## and then cached.
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        ## Finally it is printed to screen.
        m
          
}
