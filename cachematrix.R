## These functions cache the inverse of a matrix. 

## This function generates a list, which contains functions to: 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value inverse of the matrix 
## 4. get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks whether the inverse of the matrix x has already been computed. 
## If so, it returnes the chached value. If not, it computes the inverse of the matrix 
## and prints it. It assumes that the matrix x is invertible. 


cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
      message("getting cached data")
      return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i

}
