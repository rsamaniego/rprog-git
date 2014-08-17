## The function makeCacheMatrix takes a matrix and makes a "special
## copy of it, which allows the inverse to be stored

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {  #set value of x
    x <<- y
    inv <<- NULL
  }
  get <- function() x #return it's value
  setinv <- function(inverse) inv <<- inverse #setinv and getinv allow the new matrix x 
  getinv <- function() inv                    #to cache the inverse
  list (set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## the function cacheSolve returns the inverse of the matrix
## if the inverse has already been calculated, it gets it from the cache
##otherwise, it calcultaes the inverse and stores it for future use

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}

##Here is an example of how the functions work
#Example
mdat <- matrix(c(1,2,3, 11,12,13, 2,4,5), nrow = 3, ncol = 3, byrow = TRUE)
x <- makeCacheMatrix (mdat)
solve(mdat)
cacheSolve(x)
cacheSolve(x) #"getting cached data"
