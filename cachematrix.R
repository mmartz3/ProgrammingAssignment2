## Put comments here that give an overall description of what your
## functions do
##makeCacheMatrix -- similar to the makeVector function from exanple,
## the first function returns a list with values that allow for setting and getting
## the matrix or inverse of the matrix
##cacheSolve -- similar to cachemean example, pulls the inverse if available.
##Otherwise, solves for the inverse of the matrix provided and sets.
## Write a short comment describing this function: comments through function--

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL           ## inv starts as a null value, intended for matrix later
      set <- function(y) {    ## assigns new  function to 'set'
            x <<- y          ## sends y through to function above
            inv <<- NULL     ## if there is a new matrix, reset inv to NULL
      }
      get <- function() {x}  ## define the 'get' function - returns the matrix argument
      setInverse <- function(inverse) {inv <<- inverse} ## assigns value of inv in parent environment
      getInverse <- function() {inv}  ## gets the value of inv where called. used in cacheSolve.
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getInverse()
      if(!is.null(inv)) {    ## if inverse has been already cached, return
            message("returned cached data") 
            return(inv)
      }
      z <- x$get()            ## assigns x from previous function to z
      inv <- solve(z, ...)   ##  solves to create inverse matrix of 'z'
      x$setInverse(inv)   ## sets inverse to inv 
      inv
}
