## These two functions create a matrix and calculate its inverse.  
## Input matrix must be invertible.

## Input argument for makeCacheMatrix must be a square matrix; assign result to a variable
## Resulting variable is input argument for cacheSolve


## This function creates a list containing functions that:
##      1.  set - sets the cached matrix to the current matrix
##      2.  get - returns the cached matrix (or empty matrix if nothing cached yet)
##      3.  setinv - sets the cached inverse to the calculated inverse
##      4.  getinv - returns the cached inverse

makeCacheMatrix <- function(mat = matrix()) {
 
     inv_mat <- NULL
  
     set <- function(y) {
          mat <<- y
          inv_mat <<- NULL
     }
  
     get <- function() mat
  
     setinv <- function(inverse) inv_mat <<- inverse
  
     getinv <- function() inv_mat
  
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}



cacheSolve <- function(x, ...) {
  
     inv_mat <- x$getinv()
  
     if(!is.null(inv_mat)) {                 ## Return cached matrix inverse if it exists
          message("getting cached data")
          return(inv_mat)
     }
  
     data <- x$get()                         ## Calculate and return matrix inverse if it does not already exist
     inv_mat <- solve(data, ...)
      x$setinv(inv_mat)
     inv_mat
}
