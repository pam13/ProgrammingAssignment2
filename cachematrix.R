## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setInverse<-function(inverse) m<<- inverse
  getInverse<-function() m
  list(set=set,
       get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}



## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getInverse()
  if(!is.null(m)){
    message("getting an inversed matrix")
    return(m)
  }
  data<-x$get()
  m<-solve(data, ...)
  x$setInverse(m)
  m
}

test_matrix<-matrix(rnorm(12),3,3)
test_matrix
testmatInv<-makeCacheMatrix(test_matrix)
cacheSolve(testmatInv)