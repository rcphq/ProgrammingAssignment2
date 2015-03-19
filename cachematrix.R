## Creating a set of functions to create a cache-friendly matrix
##  the matrix needs to be created "with" makeCacheMatrix
##  after that, the inverse of said matrix should be accessed via cacheSolve
##  Sample Code:
##    my_m <- matrix(c(1,2,3,4),nrow=2,ncol=2)
##    t <- makeCacheMatrix(my_m)
##    cacheSolve(t)
##    cacheSolve(t) #this call should display cache data


## Creates a matrix that caches its inverse
## USAGE: cachedMatrix <- makeCacheMatrix(your_matrix)
## NOTES: Access the cached inverse via cacheSolve(cachedMatrix)
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {x}
  setinverse <- function(inv)  { m <<- inv}
  getinverse <- function() {m}
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the inverse of a makeCacheMatrix using cache or live data as needed
## USAGE: inversed <- cacheSolve(cachedMatrix)
## NOTES: Create the cached matrix via makeCacheMatrix(your_matrix)
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if (!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
