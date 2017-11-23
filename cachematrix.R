## These functions calculate the inverse of a matrix if the inverse is not yet calculated 
## or return the inverse of a matrix from cache in case that the inverse was already calculated
## 

## makeCacheMatrix takes matrix as a parameter, then it initializes the inverse matrix (i)
## and cvreates 4 functions: set (gives a new value to the matrix x), 
## get (returns matrix x), 
## setinverse (gives a new value to the inverse of matrix x, stored in i), 
## getinverse (returns the value of i)
## The function returns a list of these 4 functions

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve takes the output of the previous function as parameter 
## and arbitrary number of other parameters to be used in the solve function.
## It checks if the inverse has been calculated
## in case it was, it just retrieves previously calculated value from cache
## If it wasn't, it calculates the inverse of matrix using solve() function 
## and caches the inverse values

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
