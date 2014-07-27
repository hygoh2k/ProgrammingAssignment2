

# this code shows how to return the inverse matrix with caching method
#  code sample
# > a<-makeCacheMatrix(matrix(c(1,2,3,4),2,2))
# > cacheSolve(a)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(a)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5



## creates a special "matrix" object that can cache its inverse.
#input: a invertible matrix
#return a cacheble matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
#input: a cacheble matrix
#Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {

  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
