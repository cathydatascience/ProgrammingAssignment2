## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinvmat <- function(invmat) inv <<- invmat
  getinvmat <- function() inv
  list(set=set, get=get, 
       setinvmat=setinvmat,
       getinvmat=getinvmat)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinvmat()        ## Return a matrix that is the inverse of 'x'
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinvmat(inv)
  inv
}
