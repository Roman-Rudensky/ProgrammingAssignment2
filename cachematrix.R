## Function makeCacheMatrix creates list of four subfunctions that set matrix, get matrix, set inverted value and get inverted value 
## Function cacheSolve using function makeCacheMatrix verifies if inverted value for given matrix exists if so this value is returned
## Otherwise inverted value of matrix is calculated and stored in inv.m variable

## set function assigns the variable x with the matrix to be inverted and clears inv.m value
## get function returns the matrix
## setinv function assigns inverted matrix to inv.m variable
## getinv function returns inverted matrix

makeCacheMatrix <- function(x = matrix()) {
inv.m <- NULL
  set <- function(y) {
    x <<- y
    inv.m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv.m <<- inv
  getinv <- function() inv.m
##Here all functions arranged as a list  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
## Check if inverted matrix exists. If so, notifies with the message, returns inv.m variable and stops function execution.  
  inv.m <- x$getinv()
  if(!is.null(inv.m)) {
    message("getting cached data")
    return(inv.m)
  }
## This block is activated if there's no inverted matrix in inv.m variable.
## The matrix to be inverted is assigned to variable data
  data <- x$get()
## inverted matrix is calculated and assigned to inv.m variable. After that inverted matrix is set using setinv function and returned as function output
  inv.m <- solve(data, ...)
  x$setinv(inv.m)
  inv.m
}
}
