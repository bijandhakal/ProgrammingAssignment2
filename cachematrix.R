## This function creates a matrix objects and can cache it's inverse
makeCacheMatrix <- function(x = matrix()) { 
  ## First set inverse of matrix to null
  inverse_matrix <- NULL
  
  ## Set the matrix
  set <- function(matrix){
    x <<- matrix
    inverse_matrix <<- NULL
  }
  
  ## Get the matrix
  get <- function() x
  
  # Set the Inverse of Matrix
  setInverse <- function(inverse) inverse_matrix <<-inverse
  
  # Get the Inverse of Matrix
  getInverse <- function() inverse_matrix
  
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

## This function computes the inverse of the matrix returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the it retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  # Get the Inverse Matrix
  inverse_matrix<-x$getInverse()
  
  ## if Inverse has been calcualted return then retrun cached Inverse Matrix
  if(!is.null(inverse_matrix)){
    message("getting cached data")
    return(inverse_matrix)
  }
  
  ## If Inverse has not been calculated 
  ## Get the Matrix
  matrix <- x$get()
  
  ## Calulate the Inverse of the Matrix
  inverse_matrix <- solve(matrix)
  
  ## Cached the calulated Inverse Matrix
  x$setInverse(inverse_matrix)
  
  ## Return Calulated Inverse Matrix
  inverse_matrix
}
