## Functions to support caching of the results of inverting a matrix 

## The functions in makeCacheMatrix provide getters and setters
## for the matrix and for its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {
    x
  }
  setcache <- function(cache) {
    m <<-cache
  }
  getcache <- function() {
    m
  }
  list(set=set, get=get,setcache=setcache,getcache=getcache)
}



## Invokes an instance of makeCacheMatrix such
## that it uses its cached version of the 
## inverted matrix, if one is available; and
## stores the inverted matrix in makeCacheMatrix
## if the matrix is being inverted for the
## first time.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getcache()
  if (!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setcache(m)
  m
}
