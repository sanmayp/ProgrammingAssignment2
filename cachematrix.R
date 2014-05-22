## makeCacheMatrix functions creates cached value and 
##   implement set, get, setmatrix, and getmatrix functions
## cacheSolve functions obtain cached value if exists otherwise 
##   create inverse, cache it, and pass it as return.

## makeCachekMatrix take matrix as parameter and
##   return list of functions

makeCacheMatrix <- function(x = matrix()) {
    # Empty local variable to hold matrix 
    ma <- NULL
    
    # Set current matrix variables to parameter and null out cached value
    set <- function(y) {
      x <<- y
      ma <<- NULL
    }
    
    # Obtain matrix value
    get <- function() x
    
    # Set current value of matrix
    setmatrix <- function(matr) ma <<- matr
    
    # Get current value of matrix
    getmatrix <- function() ma

    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## Take makeCacheMatrix as parameter and return inverse matrix from cache/compute it.

cacheSolve <- function(x, ...) {
  # Retrieve matrix stored in x
  ma <- x$getmatrix()
  
  #If matrix is already cached (not null) then return inverse matrix that is cached.
  if(!is.null(ma)) {
    message("getting cached data")
    return(ma)
  }
  
  # Get matrix that is stored in ma variable using makeCacheMatrix$get function
  matrixdata <- x$get()
  # Invert matrix
  ma <- solve(matrixdata, ...)
  # Cache matrix so we can re-use matrix in next result.
  x$setmatrix(ma)
  ## Return a matrix that is the inverse of 'x'
  ma
}

##Create matrix
#x <- makeCacheMatrix(matrix(rnorm(5*5),nrow=5)) ; x$get()
##Get inverse matrix
#inversex <- cacheSolve(x) ; inversex ## Computing inverse of matrix and caching its result and pass result
#inversex <- cacheSolve(x) ; inversex ## Return Cached inverse
#x$set(matrix(rnorm(5*5),nrow=5))
#inversex <- cacheSolve(x) ; inversex ## Computing inverse of matrix and caching its result and pass result
