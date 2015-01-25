## chahed matrix which can store its inverse matrix, thus when calculating
## its inverse for the second time, we can use the stored value.

## constructor of the cached matrix, with four methods, namely
## set, get, setinv, getinv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(ori_mat){
    x <<- ori_mat
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(out_inv) inv <<- out_inv
  getinv <- function() inv
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## solve the inverse matrix of given cached matrix, using 
## the stored value if avaliable.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
