## The function makeCacheMatrix takes an argument forcing it to be a matrix
## It initializes the variable inv to be NULL. 
## It creates a subfunction set that takes the argument y and sets it to x. 
## It creates a subfunction get that returns x
## It creates a subfunction setinv that solves the inverse of the matrix. 
## It creates a subfunction getinv that returns the value of inv

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## The function cacheSolve takes the variable x and uses the getinv
## subfunction to see it have a value for inv
## The if statement checks to see in the value is null
## If the value is not null, it returns the variable inv that is stored
## If the value is null, it solves the inverse, places the variable in
## memory, and returns the output

cacheSolve <- function(x, ...) {
  
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
