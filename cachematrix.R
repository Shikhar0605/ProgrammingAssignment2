## Caching the inverse of a Matrix:
## Below are a pair of functions that we can use to store a matrix 
## and caches its inverse



makeCacheMatrix <- function(x = matrix()) {
  j<-NULL
  set<-function(y){
    x<<-y
    j<<-NULL
  }
  get<-function()x
  setInverse <- function(inverse)j<<-inverse
  getInverse<-function()j
  list(set=set,
       get=get,
       setInverse= setInverse,
       getInverse= getInverse)

}


## The below function computes the inverse of the matrix created
## by makeCacheMatrix. If the inverse has already been calculated
## then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        j<-x$getInverse()
        if(!is.null(j)){
          message("getting cached data")
          return(j)
        }
        mat<-x$get()
        j<-solve(mat,...)
        x$setInverse(j)
        j
}

