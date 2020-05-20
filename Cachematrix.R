## For this assignment I just followed the example code given in the instrution 


## This function creates a special object that can cache its inverse.
## this object is really a list

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function ()x
  setinv <- function(inv) m <<- inv
  getinv <- function () m
  list(set=set,
       get=get,
       setinv=setinv,
       getinv=getinv)

}


## This function computes the inverse of the special object returned by the function above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if (!is.null(m)) {
    return(m)
  }
  d <- x$get()
  m <- solve(d, ...)
  x$setinv(m)
  m
}
