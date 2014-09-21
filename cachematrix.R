## calculate the inverse of a matrix, through a special function that can 
## create a matrix object and cache its inverse

## utility matrix function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # set function
  set <- function(y){
    x <<- y
    inv <<-NULL
  }
  # get function
  get <- function() x
  # set the already calculated inverse
  setInv <- function(n) inv <<- n
  # get the saved inverse
  getInv <- function() inv
  # return list
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## return the inverse of a matrix x, through either calculation or get from cache

cacheSolve <- function(x, ...) {
  inv <- x$getInv();
  if(!is.null(inv)){
    message("chached data");
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  inv
}
