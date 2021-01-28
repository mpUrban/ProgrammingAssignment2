# makeCacheMatrix returns 4 functions to the global env
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the matrix inverse
# 4. get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #initializing storage location for matrix inverse
  set <- function(y) {
    x <<- y #assign a value to an object in an environment that is different
    # from the current environment
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# cacheSolve: calculate the inverse of a matrix if the inverse is not already
# stored

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  # checks if the inverse of the matrix already exists.  if so, it returns
  # the inverse
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  return(inv)
}


testMat<-makeCacheMatrix(matrix(
  c(1,3,2,4), 
  nrow=2,
  ncol=2,
  byrow = TRUE))
testMat$get()
cacheSolve(testMat)

