## cachematrix.R
## Cache the matrix and its inverse value if it has been computed before, it can
## be looked up in the cache rather than recomputed of matrix every time 

## makeCacheMatrix 
## This function allows you to store the matrix and the inverse. You can 
## get - get the matrix back
## set - set the matrix
## getinverse - get the inverse back if it has been set
## setinverse - set the inverse
## 
## @param x matrix to be cached


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve
## This function allows you to solve the inverse of a square matrix. It first 
## checks if makeCacheMatrix has the cached inverse, if not it will compute the
## inverse and store it back to the makeCacheMatrix
##
## @param x makeCacheMatrix


cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        ## Return a matrix that is the inverse of 'x'
        i
}
