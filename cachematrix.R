## The two functions "makeCacheMatrix" and "cacheSolve" are used to create a special object that stores a matrix and caches its inverse.

## in the makeCacheMatrix, creates a list containing a function to set the value of the matrix (variable x), gets its value and sets(gets) the value of its inverse. It is assumed that x is invertible.

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) x_inv <<- inverse
  getinverse <- function() x_inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}




## The cacheSolve function calculates the inverse of the matrix created with the makeCacheMatrix function (if the inverse exists. If not, it gives an error).
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  x_inv <- x$getinverse()
  if(!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  x_inv <- solve(x$get())
  x$setinverse(x_inv)
  x_inv
}
