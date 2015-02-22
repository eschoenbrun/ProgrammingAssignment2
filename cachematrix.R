## The first function, makeCacheMatrix, is used to create a matrix that can cache its inverse
## The second function, cacheSolve, calculates the inverse of the matrix set by the first function,
## makeCacheMatrix. First, thought, it checks if the matrix has changed and if ont it can pull 
## the data straight from the cache and does not need to compute the inverse again

## The first function, makeCacheMatrix, creates a special "matrix", which is really a list containing a 
## function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the inverse of the matrix
## 4.  get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function calculates the inverse of the matrix created by the first function
## It first checks if the inverse of the matrix has been calculated
## If it has, it returns that inverse from the cache, and if not it runs a new computation

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- inverse(data, ...)
  x$setinv(i)
  i
}