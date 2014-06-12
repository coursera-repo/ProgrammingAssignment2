## This function takes as input a square matrix and
## returns its inverse. The input matrix is assumed
## always invertible.

## makeCacheMatrix contains a set of functions to
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the inverse matrix
##    get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInvMatrix <- function(solve) m <<- solve
        getInvMatrix <- function() m
        list(set = set,
             get = get,
             setInvMatrix = setInvMatrix,
             getInvMatrix = getInvMatrix)
}

## cacheSolve calculates the inverse matrix of the special
## supplied matrix created with makeCacheMatrix. However,
## it first checks to see if the inverse matrix has already
## been calculated. If so, it gets the inverse matrix from
## the cache and skips the computation. Otherwise, it calculates
## the inverse matrix of the data and sets the value of the
## inverse matrix in the cache via the setInvMatrix function.

cacheSolve <- function(x, ...) {
        m <- x$getInvMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInvMatrix(m)
        m
}
