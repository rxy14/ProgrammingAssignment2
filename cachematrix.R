

## Put contents here that give an overall description of what your
## functions do

##   The following function creates a 'special matrix' that helps saving the value of the inverse of a squared matrix
## without the need to re-calculate it. makeCacheMatrix returns a list of 4 functions that allow setting/getting the 
## values of the matrix & its inverse

makeCacheMatrix <- function(mat=matrix()){
  inv<-NULL
  set <- function(mat2) {
    mat <<- mat2
    inv <<- NULL
  }
  get <- function() mat
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


##   The following function calculates the inverse of the 'special matrix' if it's a new matrix & returns the previously
## calculated inverse if it has already been calculated (its value is cached) 

cacheSolve <- function(mat, ...) {
  inv <- mat$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- mat$get()
  inv <- solve(data, ...)
  mat$setinv(inv)
  inv
}