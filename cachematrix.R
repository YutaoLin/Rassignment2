
## This function solve a matrix's inverse.

## To use this funcion, one has to firstly create a makeCacheMatrix object by inputting a matrix like
##     x1<-makeCacheMatrix(matrix(rnorm(9),3,3))
## And then, cacheSolv(x1) could get the inverse of the x1. If the inverse is caculated before, the value
## would be stored and a message "getting cached data" will appear on the screen and the cached inverse shall
## be returned.

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL                        ## i is the invese we want
  set <- function(y){            ## set() is used to set the matrix value
    x<<-y
    i<<-NULL
  }
  get <- function() x            ## get() could be used to get the matrix value
  setinverse <- function(inverse) i<<-inverse  ## setinverse() is used to set the inverse of the matrix
  getinverse <- function() i                   ## getinverse() could be used to get the inverse of the matrix
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse) ## return a list containing all the objects
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<-x$getinverse()                    ## firstly, check if the inverse is caculated before
  if(!is.null(i)){                     ## if yes, return the cached data
    message("getting cached data")
    return(i)
  }
  data<-x$get()                       ## if no, return the new caculated outcome and cache it.
  i<-solve(data,...)
  x$setinverse(i)
  i
}
