##cachematrix.R
##Here findeth thee functions to handle storage of both a matrix and its inverse,
##such that the inverse not needeth be computed every time it is wanted

##makeCacheMatrix: handles storing and retreiving of a matrix, henceforth
##referred to as x, as well as the inverse of x
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL #clear the inverse matrix
  #set the value of the matrix x, clear the inverse matrix inv
  set <- function(y){
    x<<-y
    inv<<-NULL
  }
  get <- function() x
  setInverse <- function(xInverse) inv <<- xInverse
  getInverse <- function() inv
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


##Returns cached inverse matrix if available.
##Computes the inverse and stores it if not available
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached inverse matrix")
    return(inv)
  }
  message("computing and storing inverse matrix")
  data <- x$get()
  inv <- solve(data,...)
  x$setInverse(inv)
  inv
}