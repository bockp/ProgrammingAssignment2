## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix that comes packaged with the possibility so store it's inverse.

makeCacheMatrix <- function(x = matrix()){
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


## This function takes a matrix created with makeCacheMatrix and returns the inverse. Beforehand, to save on calculation time, it checks if the matrix has an inverse associated with it in the cache. If it has, then it returns said inverse. If not, it calculates the inverse and returns it, while also storing a copy in the cache for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m

}
