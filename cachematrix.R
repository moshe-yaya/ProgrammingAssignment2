## makeCacheMatrix is a function that can save the invers matrix of hersalf (whit cacheSolve function)
##martix$get()           Returns original matrix
##matrix$getinverse()    Returns matrix inverse

makeCacheMatrix <- function(x = matrix()) {
     Inverse <-NULL
     set <- function(y) {
          x <<- y
          Inverse <<- NULL
     }
     get <- function() x
     setInverse <- function(solve) Inverse <<- solve 
     getInverse <- function() Inverse
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
     
} ##end makecacheMatrix


##cacheSolve(matrix)     Returns cached matrix inverse 


cacheSolve <- function(x, ...) {
     Inverse <- x$getInverse()
     if(!is.null(Inverse)) {
          message("getting cached data")
          return(Inverse)
     }
     data <- x$get()
     Inverse <- solve(data, ...)
     x$setInverse(Inverse)
     Inverse
  
}## end cacheSolve

