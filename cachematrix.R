## These functions will use lexical scoping and the cache to store a complex matrix operation (getting the inverse of a matrix) in such a way that once made,
## it needs not be recalculated each time.
##
## Instead, the special matrix type caches the result of solve(), which can then be returned by the cacheSolve function.
## If there is no cached result, cacheSolve will calculate the inverse of the matrix and store it in the cache.

## And yes, I DID just copy and modify the original code for makeVector and cacheMean to adapt them to my purposes (AKA., the matrix inversion).
## 


## This function creates a special matrix that comes packaged with the possibility so store it's inverse.

makeCacheMatrix <- function(x = matrix()){
  
  # initialize m as NULL, seeing as we're creating a new special matrix with no infromation yet in the cache.
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
