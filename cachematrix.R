## This function creates a special "matrix" object that can cache its inverse.
##it uses the special assignment operator <<- to create variables in its own environment 
## This function takes the matrix and caches it using setsolve. getsolve function holds the inverse of a matrix. 

makeCacheMatrix <- function(x = matrix()) {
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


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## In the invesre is already calculated and not changes, the functions retrieves the inverse from the cash.
## If inverse is not available,  it computes the inverse and sets the inverse in cache using the previous functions

cacheSolve <- function(x, ...) {
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