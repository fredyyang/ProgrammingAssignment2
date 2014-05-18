## Just using the same structure as makeVector() and cachemean()
## Create a pair of functions makeCacheMatrix() and cacheSolve()
## Call them as below:
## > source("cachematrix.R")
## > matrixA<-makeCacheMatrix(matrix(c(1:8,10),3,3))
## > cacheSolve(matrixA)
## > cacheSolve(matrixA)
## Cache would be used when calling cacheSolve() the second time

## MakeCacheMatrix() creates a list that stores a matrix and its inverse  
makeCacheMatrix <- function(x = matrix()) {
      inv<-NULL
      set<-function(y){
            x<<-y
            inv<<-NULL
      }
      get <- function() x
      setinverse<-function(inverse) inv <<- inverse
      getinverse<-function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## cachesolve() looks for inverse in the cache fisrt. If null, use solve() instead and cache the result
cacheSolve <- function(x, ...) {
      ## get inverse from cache 
      inv <- x$getinverse()
      ## if a cache exists, just return the cache value without calculation
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      ## if no cache, use solve() to calculate the inverse
      data <- x$get()
      inv <- solve(data, ...)
      ## set the inverse to cache
      x$setinverse(inv)
      inv
}
