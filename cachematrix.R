
## function "MakeCacheMatrix" creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL    #  - default to NULL
  
  ## set function is rarely used, often to assign a new value to object
  ## by saving the new matrix y sent by the call
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## get function returns the value of the original matrix
  get <- function() {x}
  
  ## setinv function will assign the inv variable to carry the inverse and reside in memory  
  ## after the first creation by function "cacheSolve"
  setinverse <- function(inverse) { inv <<- inverse }
  
  ## getinv function will simply retrieve the inverse from the memory 
  getinverse <- function() { inv }

  ## accessed at each call to makeCacheMatrix to inform the caller about the
  ## internal procedures inside the function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## function "cacheSolve" computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
##    then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(xm, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  ## if there is an inverse in cache , return its value
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## if no inverse is found, start to solve for the inverse 
  data <- x$get()
  inv <- solve(data, ...)
  ## store the inverse in the original variable x for further use 
  x$setinverse(inv)
  inv
}
