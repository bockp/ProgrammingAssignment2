## These functions will use lexical scoping and the cache to store a complex matrix operation (getting the inverse of a matrix) in such a way that once made,
## it needs not be recalculated each time.
##
## Instead, the special matrix type caches the result of solve(), which can then be returned by the cacheSolve function.
## If there is no cached result, cacheSolve will calculate the inverse of the matrix and store it in the cache.

## And yes, I DID just copy and modify the original code for makeVector and cacheMean to adapt them to my purposes (AKA., the matrix inversion).
## 


## This function creates a special matrix that comes packaged with the possibility so store it's inverse.

makeCacheMatrix <- function(x = matrix()){
  
  m <- NULL
  # initialize m as NULL, seeing as we're creating a new special matrix with no information yet in the cache.
  
  set <- function(y) {
    x <<- y
    m <<- NULL
    # defines x, and changes m, in the parent environment of the set function, which can be referred to as our "cache",
    # (After what I understand, it's simply the working environment).
    
  }
  get <- function() x
  # This is a simplified version of writing function(){x}, AKA., a function that takes no arguments and just returns x.
  
  setsolve <- function(invMatrix) m <<- invMatrix
  # This function sets m to the value of the inversed matrix invMatrix. NOTE: it changes m *OUTSIDE* this function, not just inside!
  
  getsolve <- function() m
  # a function that returns m.
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
  # makes the new special matrix object subsettable by assigning a name to each of the defined functions (though "set = set, get = get" etc., looks plain weird...)
  
  
  
  
  
  
}


## This function takes a matrix created with makeCacheMatrix and returns the inverse.
## Beforehand, to save on calculation time, it checks if the matrix has an inverse associated with it in the cache.
## If it has, then it returns said inverse. If not, it calculates the inverse and returns it, while also storing a copy in the cache for future use.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        
  m <- x$getsolve()
  
  # Get's the m variable from the cache and stores it in a local variable, also named m, inside the function. 
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
    
    # if m is not NULL, AKA. if cacheSolve has been used beforehand on this specific matrix,
    # then it prints "getting cached data" and returns m, which also stops the rest of the cacheSolve function from executing !
  }
  data <- x$get()
  # stores the function "get" (the one that just returns x) in the variable data.
  
  m <- solve(data, ...)
  # Stores the inverse matrix obtained through solve in the local variable m.
  
  x$setsolve(m)
  # set's the CACHED value of m to the newly calculated value contained in the LOCAL m variable.
  
  m
}
