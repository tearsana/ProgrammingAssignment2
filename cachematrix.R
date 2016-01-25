#makeCacheMatrix
## Creates a special matrix object that can cache its inverse
## Does 4 things
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  m.inv <- NULL  # sets the inveerse to null
  set <- function(y) {  # Setes the maxtrix x to a new matrix y, and sets the inverse to null
    x <<- y
    m.inv <<- NULL
  }
  get <- function() x  # returns the matrix x
  setinv <- function(inverse) m.inv <<- inverse  # sets the inverse m.inv to matrix.inverse
  getinv <- function() m.inv  # returns the matrix inverse m.inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve
## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Quits if matrix is not square
  ## Return a matrix that is the inverse of 'x'
  m.inv <- x$getinv()
  if(!is.null(m.inv)) {
    message("Getting cached data.")
    return(m.inv)
  }
  data<-x$get()
  if(nrow(data)==ncol(data)){
    m.inv <- solve(data, ...)
    x$setinv(m.inv)
    return(m.inv)
  } else {
    message("Matrix is not a square matrix.")
  }
}
