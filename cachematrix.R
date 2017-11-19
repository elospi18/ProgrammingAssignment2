## This Function Creates a Cache inverse matrix: first we store the data in the cahce and then we sove the input matrix
## 

## Functions uses as input a object that is a inversible matrix

makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL
  set <- function(y) {
    x <<- y
    iv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) iv <<- inverse
  getinverse <- function() iv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This Function solves the Cache Matrix stored we assume also x is a inversible matrix

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data.")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

