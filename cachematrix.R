## Creating Cache for Inverse of a matrix
## Below functions are used to creat a special object to store a matrix and cache it's inverse



makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Below function calculates the inverse of a special "matrix" created by
##makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  dat <- x$get()
  inv <- solve(dat,...)
  x$setInverse(inv)
  inv
}
