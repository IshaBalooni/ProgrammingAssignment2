## The program computes the cached inverse of a matrix (assuming the matrix to be square)

## makeCacheMatrix() creates an R object that stores a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) 
{
  I<-NULL
  set<-function(y)
  {
    x<<-y
    I<-NULL
  }
  
  get<-function()
    x
  setinverse<-function(inverse)
    I<<-inverse
  getinverse<-function()
    I
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## cacheSolve() takes an argument returned by makeCacheMatrix() in order to retrieve the 
## inverse from the cached value that is stored in the makeCacheMatrix() object's environment.


cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
  I<-x$getinverse()
  if(!is.null(I))
  { 
    message("Getting cached Inverse")
    return(I)
  }
  
  data<-x$get()
  I<-solve(data,...)
  x$setinverse(I)
  I
}
