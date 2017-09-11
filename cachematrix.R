## Put comments here that give an overall description of what your
## functions do

## make matrix function makes matrix and list of set and get function

makematrix <- function(x = matrix()) {
    inverse_matrix <- NULL
    set <- function(y) {
        x <<- y
        inverse_matrix <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inverse_matrix <<- inv
    getinv <- function() inverse_matrix
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}



## Cacheinv calculates inverse of the matrix and cathes in in memory to return next time

cacheinv <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    mat.data <- as.matrix(x$get())
    #View(data)
    m <- solve(mat.data, ...)
    x$setinv(m)
    m
}


