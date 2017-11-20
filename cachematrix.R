## These functions compute the inverse of a matrix and store the result.
## If the matrix inverse has already been found, the function returns
## the inverse without re-computing it

## this function creates a list object that allows you to store the
## inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  
  set <- function(y) {  #
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(calc_inverse) inv <<- calc_inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## If the inverse of the matrix has already been calculated,
## return the inverse
## If the inverse has not already been calculated
## calculate the inverse and cache it 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  my_inv <- x$getinv()
  if(!is.null(my_inv)){
    message("getting cached data")
    return(my_inv)
  }
  data <- x$get()
  my_inv <- solve(data)
  x <- x$setinv(my_inv)
  my_inv
  }
