## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
                 i <- NULL              ## i is undefined as of now
                 set <- function(y) {
                        x <<- y         ## y goes to x (parent envionment)
                        i <<- NULL
}
                get <- function() x     ## function to 'get' the data which passed through 'set'
                setinverse <- function(inverse) i <<- inverse   ## setinverse is set by passing through inverse function 
                getinverse <- function() i
                list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

## Write a short comment describing this function

cacheSolve <- function(x, ...) {                        
        i <- x$getinverse()                             ## i is retreived from getinverse function of x from makeCacheMatrix
        if(!is.null(i)) {                               ## return i if the function has cached amount 'i'
                message("getting the cache data")
                return(i)
        }
        data <-x$get()                                  ## if there's no cached data, find the data from x$get of function x
        i <-solve(data,...)                             ## i is retruned by passing through sole function
        x$setinverse(i)                                 ## return a matrix that is the inverse of 'x'
        i
}
