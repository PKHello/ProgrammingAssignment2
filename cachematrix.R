## The following two functions 'cache' the costly computation of Matrix Inversion.  
## If the Matrix Inverse in needed again, then it can be looked up in the cache rather than recomputed.


## The 'makeCacheMatrix' function takes matrix as input and  returns a list.
## When the function is called to create special matrix, it initializes inverse to Null and caches matrix
## It defines functions to set matrix, get matrix, set Invers of matrix and get inverse of matrix


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                                        ## initialize inverse to Null
  set <- function(y) {
    x <<- y                                        ## Cache the matrix
    i <<- NULL                                     ## initialize inverse to Null
  }
  
  get <- function() x                              ## get function returns the cached matrix
  setInverse <- function(inverse) i <<- inverse    ## setinverse function caches the inverse
  getInverse <- function() i                       ## getinverse function returns the inverse
  list(set = set, get = get,                       ## returns the list
       setInverse = setInverse,
       getInverse = getInverse)

}


## The 'cacheSolve' functon checks if the inverse was already calculated using getInverse function. 
## If inverse was already calculated and cached, then it returns the cached inverse and displayes message
## If inverse was NOT earlier calculated, then it calculates inverse using SOLVE and calls setInvers so that
##    the calculated inverse is cached and need not be calculated againWrite a short comment describing this function

cacheSolve <- function(x, ...) {
  
  i <- x$getInverse()
  
  if(!is.null(i)) {      # check if Inverse was already calculated and cached
    message("getting cached data")
    return(i)            # since inverse was already cached, display message and return cached inverse of matrix
  }
  
  data <- x$get()        # get the matrix
  i <- solve(data, ...)  # calculate inverse of matrix for the first time
  x$setInverse(i)        # set inverse in cache so that next time it need not be calculated 
  i                      # Return the inverse matrix
}
