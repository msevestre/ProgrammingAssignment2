#The functions defined below are used to create a special "matrix" object that stores a matrix and caches its inverse when computed.
# - Use 'makeCacheMatrix' to create a matrix with cache
# - Use 'cacheSolve' to retrieve or calculate the inverse of a special "matrix" object and store its cache


#' makeCacheMatrix: Creates a special "matrix" object that can cache its inverse
#'
#' @param x: a matrix
#'
#' @return a special "matrix" object that can cache its inverse. It is a simple list providing access to the following functions
#'  set: Sets the matrix. Note: calling this function also sets the cached inverse to NULL.
#'  get: Returns the matrix.
#'  setinverse: Sets the inverse matrix.
#'  getinverse: Gets the inverse matrix. Returns NULL if the inverse was not calculated 
makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
      x <<- y
      inv <<- NULL
   }
   get <- function() {
      x
   }
   setinverse <- function(inverse) {
      inv <<- inverse
   }
   getinverse <- function() {
      inv
   }
   list(set = set, 
        get = get, 
        setinverse = setinverse,
        getinverse = getinverse)
}



#' cacheSolve: Calculates the inverse of a special "matrix" object created with the function 'makeCacheMatrix'. 
#' If the inverse has already been calculated, the inverse is retrieved from the internal cache and the computation
#' returns immediately. Otherwise it calculates the inverse and stores it in the cache before returning it.
#' NOTE: We assume that the matrix x is ALWAYS invertible
#'
#' @param x : A special matrix object created with makeCacheMatrix for which the inverse should be calculated
#' @param ... : further arguments passed to the solve function
#'
#' @return the inverse of the matrix used to create the special "matrix" 'x'
cacheSolve <- function(x, ...) {
   inv <- x$getinverse()
   if(!is.null(inv)) {
      message("Retrieving inverse from cache")
      return(inv)
   }
   mat <- x$get()
   inv <- solve(mat, ...)
   x$setinverse(inv)
   inv
}
