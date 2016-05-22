## makeCacheMatrix creates a special matrix to store the contents of a matrix and its inverse.
## cacheSolve  returns the  inverse of the matrix, either by initial calculation of the inverse or by using 
## a previous calculated version of the inverse.  


## The  makeCacheMatrix creates a special "matrix", which is really a list containing a function to:
## 
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    x.inverse <- NULL
    set <- function(y) {
      x <<- y
      x.inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inversemat) x.inverse <<- inversemat
    getinverse <- function() x.inverse
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via 
## the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  xinv <- x$getinverse()
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  data <- x$get()
  xinv <- solve(data,...)
  x$setinverse(xinv)
  xinv
}
