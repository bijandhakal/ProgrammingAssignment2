## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { 
  inverse_matrix<-NULL
  set<-function(matrix){
    x<<-matrix
    inverse_matrix<<-NULL
  }
  get<-function() x
  setReverse<-function(inverse) inverse_matrix <<-inverse
  getReverse<-function() inverse_matrix
  
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}
cacheSolve <- function(x, ...) {
  inverse_matrix<-x$getInverse()
  if(!is.null(inverse_matrix)){
    message("getting cached data")
    return(inverse_matrix)
  }
  matrix <- x$get()
  inverse_matrix <- solve(matrix)
  x$setInverse(inverse_matrix)
  inverse_matrix
}
