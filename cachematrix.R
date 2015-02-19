# Creat makeCacheMatrix to feed cacheSolve a list of functions to calculate an inverted matrix
makeCacheMatrix <- function(x = matrix()) {
    # store the cached value and clear cache
    matrixcache <- NULL
    # create matrix in environment
    set <- function(y) {
        x <<- y
        matrixcache <<- NULL
    }
    # get value of matrix
    get <- function() x
    # invert matrix to store in cache
    setMatrix <- function(invert) matrixcache <<- invert
    # retrieve inverted matrix from cache
    cacheInverse <- function() matrixcache
    # return functions to environment
    list(set = set, get = get, setMatrix = setMatrix, cacheInverse = cacheInverse)
}

# Create cacheSolve to invert matrix in makeCacheMatrix
# If matrix is not there create matrix
cacheSolve <- function(x, ...) {
    # get inverse of matrix
    matrixcache <- x$cacheInverse()
    # get inverted matrix if already exists or invert matrix
    if (!is.null(matrixcache)) {
        message("Getting Cached Data")
        # return matrix
        return(matrixcache)
    }
    # generate new matrix
    matrix <- x$get()
    
    # Return final answer
    matrixcache <- solve(matrix, ...)
    x$setMatrix(matrixcache)
    return (matrixcache)
}
