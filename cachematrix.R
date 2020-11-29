## define a function that creates a matrix object that can cache its inverse##

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(n){
    x <<- n
    m <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

##define a function that computes the inverse of the matrix created above##
cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat,...)
  x$setInverse(m)
  m
}
