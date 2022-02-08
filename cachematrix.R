## Put comments here that give an overall description of what your
## functions do

## The following function creates an variable which stores a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        k <- NULL
        set <- function(y) {
                x <<- y
                k <<- NULL
        }
        get <- function() x
        setinv <- function(inv) k <<- inv
        getinv <- function() k
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)

}


## computes the inverse of the matrix

cacheSolve <- function(x, ...) {
        k <- x$getinv()
        if (!is.null(k)) {
                message("using cached")
                return(k)
        }
        data <- x$get()
        k <- solve(data, ...)
        x$setinv(k)
        k
        ## Return a matrix that is the inverse of 'x'
}
