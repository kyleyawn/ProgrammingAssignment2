## Calculates the inverse of a matrix. If the inverse has already been
## calculated, then it pulls the result from the cache.

## Creates a special "matrix", which is really a list containing a function
## to 
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(m = matrix()){
  i <- NULL
  set <- function(y) {
    m <<- y
    i <<- NULL
  }
  get <- function() m
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Calculates the inverse of the "matrix" created with makeCacheMatrix
## Checks to see if it has already been calculated first.

cacheSolve <- function(m, ...) {
  i <- m$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- m$get()
  i <- solve(data, ...)
  m$setinv(i)
  i
}
