## Put comments here that give an overall description of what your
## functions do

## Creates a list containing a function which will:
## 1) Set value of matrix
## 2) Get value of matrix
## 3) Set value of inverse of the matrix
## 4) Get value of inverse of the matrix

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


## Returns the inverse of the matrix. Checks to see if inverse has already been computed.
## If not, will compute the inverse and sets the value within the cache.
## If so, will just get the result and skip the computation

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
