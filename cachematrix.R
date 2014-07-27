## These two functions correspond to programming assingment 2. It's main goal is to 
## calculate the inverse of a matrix using caching strategy to reduce computation time.

## Function makeCacheMatrix returns a wrapped matrix one main feature: it caches the results
## of its inversion

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function cacheSolve calculates the inverse of a matrix, but first it checks wheter it
## has been called before or not. It it was called before, the results will be stored in the
## matrix (created by makeCacheMatrix) cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

