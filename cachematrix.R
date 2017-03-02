## Creating Cache for Inverse of a matrix
## Below functions are used to creat a special object to store a matrix and cache it's inverse



makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ##Initilizing Inv
  set <- function(y) ## Setiing value of the matrix
  {
    x <<- y
    inv<- NULL
  }
  get <- function() x ##get value of the matrix
  setInverse <- function(inverse) inv <<- inverse ## Get the inverse value
  getInverse <- function() inv
  list(set = set,
       get = get,    ## Return the list
       setInverse = setInverse,
       getInverse = getInverse) 

}


## Below function calculates the inverse of a special "matrix" created by
##makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse() ##If the inverse has already been calculated
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv) ## retrieve the inverse from the cache
  }
  dat <- x$get() #if not, load the new matrix
  inv <- solve(dat,...) #and calculate the inverse
  x$setInverse(inv) # Store the value in Cache
  inv ## Return a matrix that is the inverse of 'x'
}
