## This function takes in a matrix as the input parameter and returns a list of functions which can be used to set the value of the matrix, get the matrix, calculate the inverse of the matrix and get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- solve(x)
  getinverse <- function() m
  
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)

}


## This function gets the inverse of the matrix created by the function makeCacheMatrix
## It takes as input the list created by the makeCacheMattrix , checks if the inverse has already been calculated in the makeCacheMatrix
##If yes, it just returns that inverse from cache(memory). If not, it calculates the inverse on its own and returns the inverse
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
