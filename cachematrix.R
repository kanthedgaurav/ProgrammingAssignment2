#Caching the Inverse of a Matrix Program

# makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  iom <- NULL
  #set the value of the matrix
  set <- function(y) {
    x <<- y
    iom <<- NULL
  }
  #get the value of the matrix
  get <- function() x
  #set the value of the inverse of matrix
  setinverseofmatrix <- function(inverseofmatrix) iom <<- inverseofmatrix
  #get the value of the inverse of matrix
  getinverseofmatrix <- function() iom
  list(set = set, get = get,
       setinverseofmatrix = setinverseofmatrix,
       getinverseofmatrix = getinverseofmatrix)
}


# cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
  #get the inverse of matrix 
  iom <- x$getinverseofmatrix()
  #checking if cached data available
  if(!is.null(iom)) {
    message("getting cached data")
    return(iom)
  }
  #stroe matrix into data variable
  data <- x$get()
  #Computing the inverse of a square matrix using the solve function in R
  iom <- solve(data,...)
  #set the iverse of matrix 
  x$setinverseofmatrix(iom)
  # Return a matrix that is the inverse of 'x'
  iom
}



