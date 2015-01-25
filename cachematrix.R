## These functions take advantage of an useful feature of R to avoid
## re-computing some time-consuming operations by caching the value we
## already computed before

##
## this function will create a special object which store a matrix
## and contain a list of functions:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse matrix
## 4) get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y){
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) s <<- solve
    getinverse <-function() s
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## 
## this function will compute the inverse of the matrix created with
## the above function. However, it first checks to see whether we already
## computed the inverse matrix or not. If yes, it get the inverse matrix
## from the cache and skip the computations. If no, it computes the 
## inverse matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getinverse()
    if(!is.null(s)){
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinverse(s)
    s
}
