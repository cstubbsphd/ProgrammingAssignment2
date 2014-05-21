## Set of functions that calculate matrix inverses.  Previously calculated 
## inverses are cached so they do not have to be recalculated (memoization)

## This function defines a list of functions (set, get, setinverse, getinverse)
## to store the matrix and its inverse
## eg:  amatrix <- makeCacheMatrix(c(1,2,3,4), nrow=2, ncol=2)
## To modify the matrix, use
##    amatrix$set(c(0,5,99,66), nrow=2, ncol=2)
## To show the matrix, use
##    amatrix$get()

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the inverse of a matrix.  It caches the inverse for
## future use, and returns the cached inverse if available.  
## eg:  inv.matrix <- cacheSolve(amatrix)
## After a call to cacheSolve, the computed inverse can be returned with
##    amatrix.getinverse()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
