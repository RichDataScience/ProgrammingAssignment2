## makeCacheMatrix creates an object that matrix stores a matrix and can be 
## acted on to by the cache solve matrix
## 

## makeCacheMatrix takes a matrix, stores it for retieval as well as exist as a "type"
## that allows for the cachesolve to work on it, storing a cached version.
## getSolve returns data if it is found

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m 
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## Takes the function makeCacheMatrix sets the inverse by calling the in functions $getsolve
## setsolve
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
   
}
