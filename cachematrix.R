## Put comments here that give an overall description of what your
## functions do

## Creates a matrix that will cache the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(
    set = get,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## cacheSolve will output the calculation of the matrix above. 
##If the inverse already exists and there are no deltas then it 
##would grab the value from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <-x$getInverse()
  if (!is.null(inv)){
    message("Retrieving cached value.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
