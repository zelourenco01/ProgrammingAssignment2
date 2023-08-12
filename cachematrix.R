## The makeCacheMatrix function creates a special "matrix" object that caches
## it's inverse.

makeCacheMatrix <- function(x = matrix()) { 
  # erasing cached data
  m <- NULL
  
  # set value of matrix
  set <- function(y) {
    x <<- y 
    m <<- NULL
  }
  
  # get value of matrix
  get <- function() x
  
  # set value of inverse matrix
  setinv <- function(solve) m <<- solve
  
  # get value of inverse matrix
  getinv <- function() m
  
  # named elements in list allows use of $ to access functions
  list(set = set, 
       get = get, 
       setinv = setinv,
       getinv = getinv)
}

## The cacheSolve computes the inverse of the "matrix" returned by.
## If inverse is already calculated, it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ....)
  x$setinv(data)
  m
}
