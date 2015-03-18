## The following functions create a new matrix object (makeCacheMatrix) capable 
## of caching its inverse, and perform the computation of the inverse only in case
## it hasn't been computed yet.


## makeCacheMatrix defines the new matrix object. It returns a list of functions 
## to get and set the matrix and its inverse. Note that the "set" function 
## resets the inverse to NULL at any re-initialization of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inverseMatrix <<- inverse
    getInverse <- function() inverseMatrix 
    list(set = set, get = get, setInverse = setInverse, 
         getInverse = getInverse)
}


## cacheSolve computes the inverse of the matrix returned by the makeCacheMatrix
## object, caches it via the makeCacheMatrix$setInverse function and returns it. 
## In case the inverse matrix is already present in the makeCacheMatrix object, 
## the new computation is avoided and the inverse from cache is returned.

cacheSolve <- function(x, ...) {
    if(!is.null(x$getInverse())){
        print("Inverse Matrix already computed. Getting it from cache")
        return(x$getInverse())
    }
    mymatrix <- x$get()
    inverse <- solve(mymatrix, ...)
    x$setInverse(inverse)
    inverse
}
