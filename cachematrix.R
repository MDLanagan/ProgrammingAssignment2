# A pair of functions that takes a square, invertable matrix as input,
# calculates the inverse of the matrix and caches this value.
# Allows the matrix inverse to be calculated only once for a given matrix input

## A function to a create a list to set/get the matrix and its inverse
#input to makeCacheMatrix is a square, invertable matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL     #initialise m
  set <- function(y) {
    x <<- y     #set x in parent environment
    m <<- NULL     #set m in parent environment clearing previous value
  }
  get <- function() x   #get x from parent environment
  setInverse <- function(inverse) m <<- inverse    #set cache of inverse
  getInverse <- function() m    #get m from parent environment
  #name functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Test to see if matrix inverse has been calculated, if not then calculate
#input to cacheSolve is output of makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # try to get an existing value of matrix inverse
  minv <- x$getInverse()
  #check if getting matrix inverse was successful, if so
  #then inform user that existing value of matrix inverse will be returned
  if(!is.null(minv)) {
    message("getting cached data")  
    #exit function returning current value of matrix inverse      
    return(minv)    
  }
  #if matrix inverse does not exist then calculate and return new value
  data <- x$get()
  minv <- solve(data, ...)
  x$setInverse(minv)
  minv
}
