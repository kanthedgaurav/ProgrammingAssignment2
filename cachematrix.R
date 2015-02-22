#Caching the Inverse of a Matrix Program

# makeCacheMatrix function creates a special "matrix" object that can cache its inverse
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse of matrix
#get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  iom <- NULL
  set <- function(y) {
    x <<- y
    iom <<- NULL
  }
  get <- function() x
  setinverseofmatrix <- function(inverseofmatrix) iom <<- inverseofmatrix
  getinverseofmatrix <- function() iom
  list(set = set, get = get,
       setinverseofmatrix = setinverseofmatrix,
       getinverseofmatrix = getinverseofmatrix)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  iom <- x$getinverseofmatrix()
  if(!is.null(iom)) {
    message("getting cached data")
    return(iom)
  }
  data <- x$get()
  iom <- solve(data)
  x$setinverseofmatrix(iom)
  iom
}
