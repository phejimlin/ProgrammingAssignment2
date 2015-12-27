## Matrix inversion is a costly time-consuming computation for a very big matrix.
## The solution is caching the inverse of a matrix instead of computing repeatedly.
## Following functions : makeCacheMatrix and cacheSolve implement above description.

## makeCacheMatrix function have one matrix arg, and four sub function
## set : set the value of matrix
## get : get the value of matrix
## setinv : set the value of inverse of the matrix
## getinv : get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    mat <- NULL
    set <- function(y) {
        x <<- y
        mat <<- NULL
    }
    get <- function() x
    setinv <- function(inv) mat <<- inv
    getinv <- function() mat
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve function will return the inverse of the matrix.
## Logic is that checking the inverse of the matrix is null or not.
## if null, it computes the inverse, sets the value via "setinv" and finally print out.
## if not null, it gets the message with result and skips computations.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mat <- x$getinv()
    if(!is.null(mat)) {
        message("getting cached data")
        return(mat)
    }
    data <- x$get()
    mat <- solve(data, ...)
    x$setinv(mat)
    mat
}

## Sample:
## > a<-makeCacheMatrix(matrix(rnorm(4),2,2))
## > a$getinv()
## [,1]      [,2]
## [1,] 0.319635 2.7925645
## [2,] 1.854086 0.4786864

## > cacheSolve(a)
## [,1]      [,2]
## [1,] 0.319635 2.7925645
## [2,] 1.854086 0.4786864

## > cacheSolve(a)
## getting cached data
## [,1]      [,2]
## [1,] 0.319635 2.7925645
## [2,] 1.854086 0.4786864
