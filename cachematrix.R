## These two functions create a matrix and calculate its inverse.  
## Input matrix must be invertible.
## Given an invertible matrix (b), the inverse is returned by:
##        cacheSolve(makeCacheMatrix(b))


## This function creates a list containing functions that:
##      1.  set - sets the cached matrix to the current matrix
##      2.  get - returns the cached matrix (or empty matrix if nothing cached yet)
##      3.  gatarg - returns the current (input) matrix
##      4.  setinv - sets the cached inverse to the calculated inverse
##      5.  getinv - returns the cached inverse

makeCacheMatrix <- function(mat = matrix()) {
  set <- function(y) mat_cache <<- y
  get <- function() {
    if(exists("mat_cache")) mat_cache     ## Return cached matrix if it exists
    else {                                ## First time run, mat_cache won't exist yet
      mat_cache <- matrix(0,0,0)          ## Initialize mat_cache for comparison 
      mat_cache
    }
  } 
  getarg <- function() mat
  setinv <- function(inverse) inv_cache <<- inverse
  getinv <- function() inv_cache
  list(set = set, get = get,
    getarg = getarg,
    setinv = setinv,
    getinv = getinv)
}


## This function returns the inverse of a matrix.

cacheSolve <- function(x, ...) {
  
  cur_mat <- x$getarg()                   ## Get the current matrix
  last_mat <- x$get()                     ## Get cached matrix 
  
  if(!identical(cur_mat, last_mat)) {     ## Compare cached and current matrices
    ## Current matrix is not the same as cached matrix; recalculate the inverse
    x$set(cur_mat)                        ## Set the cached matrix to the current for comparison next time
    inv_calc <- solve(cur_mat)            ## Calculate the inverse 
    x$setinv(inv_calc)                    ## Cache the calculated inverse
    return (inv_calc)                     ## Return the calculated inverse and exit
  }      
  
  ## Input matrix is the same as previous input, return cached value
  message("Input matrix same as previous - getting cached inverse.")
  x$getinv()                              ## Return the cached inverse                  
}
