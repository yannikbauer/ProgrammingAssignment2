## These functions follow precisely the logic set out by the example functions
## makeVector() and cachemean(). Analogous to makeVector(), makeCacheMatrix() creates 
## and object from its input structure (here, a matrix rather than a vector) and
## provides associated object getter and setter methods that can be called.
## Analogous to cachemean(), cacheSolve() returns the result of a computation on the
## input object. In contrast to cachemean(), which returns the mean of a vector,
## cacheSolve() returns the inverse of a matrix. If the inverse has already been
## computed and cached, and the matrix object is the same, then the cached value 
## is retrieved. Otherwise, the inverse is computed fresh.

## This function creates a matrix object from an input matrix x and can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  #' makeCacheMatrix() creates a matrix object from an input matrix x
  #' that can cache its inverse by returning a list of its accessor 
  #' methods get() and getinverse(), and the mutator methods set() and 
  #' setinverse(). Per design, makeCacheMatrix() requires the function
  #' cacheSolve() to retrieve the cached matrix inverse of populate it.
  #' NOTE: matrix must be invertible.
  #' EXAMPLE USAGE: 
  #'   x = cbind(c(1,2),c(0,1)) # <> singular: matrix(data=1:9, nrow=3, ncol=3)
  #'   mat = makeCacheMatrix(x)
  
  i <- NULL # initialize matrix inverse
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function returns the inverse of the matrix object returned by`makeCacheMatrix`
cacheSolve <- function(x, ...) {
  #' Return a matrix that is the inverse of the matrix object 'x' returned by 
  #' `makeCacheMatrix`. If the matrix inverse on x has already been computed 
  #' and x has not changed, cacheSolve() retrieves the cached value from
  #' makeCacheMatrix via the accessor method x$get(); otherwise, the inverse
  #' is computed and cached via x$setinverse()
  #' EXAMPLE USAGE
  #'   cacheSolve(mat)
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)# mean(data, ...)
  x$setinverse(i)
  i
}
