## The purpose of the two functions below is to allow the user to cache the inverse of a matrix.
#   makeCacheMatrix 

# makeCacheMatrix: create a special "matrix" object that can cache its inverse
# Has the methods set, get, setinv, getinv
makeCacheMatrix <- function(x = matrix()) {
  # initializes
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


# cacheSolve: checks if inverse has already been calculated. If so, gets inverse from cache using getinv and returns it.
#  Otherwise, it calculates the inverse using solve and caches the inverse using setinv. 
#  Therefore, if the same matrix is called again it can get it directly from cache and avoid recalculating. 
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

# Test:
X1 = makeCacheMatrix(matrix(c(-3, 1, 5, 0), nrow=2, ncol=2))
cacheSolve(X1)
cacheSolve(X1)

X2 = makeCacheMatrix(matrix(c(0,0,1,1,0,0,0,1,0), nrow=3, ncol=3))
cacheSolve(X2)
cacheSolve(X2)
