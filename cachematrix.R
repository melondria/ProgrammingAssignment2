## The functions makeCacheMatrix and cacheSolve allow for the retrieval of a
## previous calculated inverse matrix, so as to not recalculate it.
## They do so by using lexical scoping and the establishment of an environment
## by the first function, which can be later accessed by the second function.

## The function makeCacheMatrix creates a list with four functions (set, get,
## setinv, getinv) and the objects m and x.

makeCacheMatrix <- function(x = matrix()){
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## The function cacheSolve returns the value of the inverse of the matrix if it
## is already in the environment. If it is not, it is calculated.

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      return(inv)
}
