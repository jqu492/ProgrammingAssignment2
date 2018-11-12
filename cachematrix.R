## The functions below cache the inverse of a matrix which is invertible.
## 

## This function makeCacheMatrix creates a list that contains 
## a function that
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the inverse
##4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
     x <<- y
     m <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse) m <<- inverse
   getinverse <- function() m
   list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function calculates the inverse of the invertible matrix
## that is provided by the function above. It first checks to see
## if the inverse of the matrix has already been calculated. If so,
## it gets the inverse from th cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the
## inverse of that matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
