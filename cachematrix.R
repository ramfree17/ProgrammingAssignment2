## This R object provides two functions intended to solve and cache
##   the invertible matrix counterpart of a given R matrix.
##
##  NOTE: This version assumes that all provided matrices are invertible.

## The makeCacheMatrix function creates a special matrix object that can cache
##    the result of a costly invertible matrix computation. It take a normal
##    R matrix.   

makeCacheMatrix <- function(x = matrix()) {

   # holder for the inverted matrix
   matrix_inv <- NULL
   
   # function to change the matrix of this object
   set <- function(y){
      x <<- y
      matrix_inv <<- NULL
   }
   
   # function to return the contained matrix of this object
   get <- function() x
   
   # function to return the computed inverted matrix of this object
   get_inverse <- function () matrix_inv
   
   # function to set the inverted matrix of this object
   set_inverse <- function (inverse) matrix_inv <<- inverse
   
   list(set = set, get = get, 
        get_inverse = get_inverse, set_inverse = set_inverse)
  
}


## The cacheSolve(x) function computes for the invertible matrix of the specified 
##    CacheMatrix object or return a cached result if it is available.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    inverse <- x$get_inverse

    #returned object is a function, need to invoke it to test the contained value
    if ( ! is.null(inverse()) ){
       message("[INFO] Returning cached matrix.")
       return(inverse())
    }
    
    # Auxilliary info message.
    message ("[INFO] Computing for the inverse matrix...")
    
    inverse <- solve(x$get())
    x$set_inverse(inverse)
    
    inverse
    
}
