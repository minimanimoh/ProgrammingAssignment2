## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {           
                i <- NULL                  ## i is undefined as of now
                set <- function(y) {
                        x <<-y             ## y goes to x (parent environment)    
                        i <<- NULL              
}
                get <- function() x        ## Function to 'get' the data which passed through 'set' (lexical scoping)
                setinverse <- function(inverse) i <<- inverse          ## setinverse is set by passing through inverse function 
                getinverse <- function() i
                list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
   

## Write a short comment describing this function

cacheSolve <- function(x, ...) {   ## Return a matrix that is the inverse of 'x'
         i <-x$getinverse()        ## i is retreived from getinverse function of x from makeCacheMatrix      
         if(!is.null(i)) {         ## Return i if the function has cached amount 'i'      
                 message("getting the cache data")
                 return(i)
         }
        data <-x$get()             ## If there's not cached data, find the data from x$get of function x
        i<-solve(data, ...)        ## i is returned by passing through solve function 
        x$setinverse(i)            
        i
}
