## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly.  

## 1. `makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its inverse.
## 2.  `cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

## The function `makeCacheMatrix` creates a special "matrix" object that can 
## cache its inverse, which is really a list containing a function to

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse matrix
## 4.  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        set <- function(y) {
                x <<- y
                invm <<- NULL
        }
        get <- function() x
        setinvmatrix <- function(invmatrix) invm <<- invmatrix
        getinvmatrix <- function() invm
        list(set = set, get = get,
             setinvmatrix = setinvmatrix,
             getinvmatrix = getinvmatrix)


}


## The function 'cacheSolve' calculates the inverse matrix of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse matrix has already been calculated. If so, it `get`s the inverse matrix from the
## cache and skips the computation. Otherwise, it calculates the inverse matrix of
## the data and sets the value of the inverse matrix in the cache via the `setmean`
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmatrix <- x$getinvmatrix()
        if(!is.null(invmatrix)) {
                message("getting cached data")
                return(invmatrix)
        }
        data <- x$get()
        invmatrix <- solve(data, ...)
        x$setinvmatrix(invmatrix)
        invmatrix
}
