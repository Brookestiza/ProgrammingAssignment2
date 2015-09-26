## This code creates two functions--one that will calculate the inverse of a matrix
## and one that computes it (or returns a cached, previously computed one), given an input matrix

## This function creates the code to build the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    c<-NULL
    set<-function(y) {
      x<<-y
      c<<-NULL
    }
    get<-function() x
    setinverse<-function(solve)  c<<-solve
    getinverse<-function() c
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the matrix returned by the function created above 
## or just uses cached value if it already exists

cacheSolve <- function(x, ...) {
  c <- x$getinverse()
  if(!is.null(c)) {
    message("getting cached data")
    return(c)
  }
  data <- x$get()
  c <- solve(data, ...)
  x$setinverse(c)
  c
}
