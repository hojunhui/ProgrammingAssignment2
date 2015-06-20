## Put comments here that give an overall description of what your
## functions do

## Create a special "matrix", containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  in <- NULL
  set <- function(y) {
    x <<- y
    in <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) in <<- inverse
  getinverse <- function() in
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Calculate the inverse of the special "matrix" created with the above function
## First check to see if the inverse has already been calculated
## If so, it gets the inverse from the cache and skips the computation
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  in <- x$getinverse()
  if(!is.null(in)) {
    message("getting cached data")
    return(in)
  }
  data <- x$get()
  in <- solve(data, ...)
  x$setinverse(in)
  in
}
