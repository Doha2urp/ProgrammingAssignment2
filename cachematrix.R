## The following two functions will save the time by preventing repeatedly computation
## of the inversion of a square invertible matrix

## This function gets a square invertible matrix and the inversion of the matrix
## and set them in to the memory.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinv <- function(solve) s <<- solve
  getinv <- function() s
  list(set = set, get = get,setinv = setinv,getinv = getinv)
  

}


## This matrix compute the inversion of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getinv()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinv(s)
  s
}
## Sample square invertible matrix
k<-cbind(c(2,2),c(3,2))
 
Matrix<-makeCacheMatrix(k)
## for the first time its compute the inversion but after that, it always use the
## cached data of the inversion of the input matrix
cacheSolve(Matrix)
