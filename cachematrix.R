## makeCacheMatrix and cacheSolve together calculate, cache, and retrieve the inverse of a provided square matrix.
## Inverting a matrix can be computationally costly, so these functions calculate the inverse of a given matrix once,
## cache it, then check to see if the inverse has been cached once the user requests the inverse.

## To run this pair of functions, feed a square matrix to makeCacheMatrix, assign
## the output of makeCacheMatrix to an object, then feed that object to cacheSolve
## example: 
##   matr <- matrix(5:8, nrow=2, ncol=2)
##   a <- makeCacheMatrix(matr)
##   cacheSolve(a)


## makeCacheMatrix creates a special matrix so that the inverse of x, the provided matrix argument, is cached.

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL  #Sets the inverse to be null
  #set, get, setinv, and getinv are set as functions
  set <- function(y) {      #This function caches x and i to the parent environment
    x <<- y
    i <<- NULL
  }
  get <- function() x  #This function will call the provided matrix
  setinv <- function(solve) i <<- solve  #This function will calculate the inverse
  getinv <- function() i  #This function will call the inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve searches the cache for the value of the inverse and calculates the inverse is none is found cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          i <- x$getinv()  #Assigns i the cached value of the inverse from makeCacheMatrix
  if(!is.null(i)) {    #Checks to see if i is empty
    message("getting cached data")  #Alerts user that inverse was previously cached
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)  #If not cached, solve for i
  x$setinv(i)
  i
}
