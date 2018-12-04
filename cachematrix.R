
## makeCacheMatrix: This function creates a square matrix object that can cache its inverse.
## It returns a list of four functions that, (1) sets the value of the matrix (2) gets the value 
## of the matrix, (3) sets the value of inverse of the matrix, and (4) gets the value of inverse 
## of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    m_i <- NULL
    set <- function(y) {
        x <<- y
        m_i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m_i <<- inverse
    getinverse <- function() m_i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: Checks if the inverse of the matrix has already been cached. 
## If the inverse does not exist (m_i = NULL), it calculates it and sets the inverted matrix in 
## the cache using the setinverse function.
## Returns the inverse.


cacheSolve <- function(x, ...) {
    m_i <- x$getinverse()
    if(!is.null(m_i)) {
        message("getting cached data")
        return(m_i)
    }
    data <- x$get()
    m_i <- solve(data, ...)
    x$setinverse(m_i)
    m_i
} 
