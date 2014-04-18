## There are two functions below which are used to create and modify an object
## to store both a matrix and it's inverse together


## makeCacheMatrix caches accepts a matrix as a argument and
## returns a list allowing user to set/get the matrix itself,
## and set or get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {  
  
  invmatrix <- NULL

  set <- function(y) {
    x <<- y
    invmatrix <<- NULL
  }
  
  get <- function () x
  
  
  setInv <- function(inverse) invmatrix <<- inverse
  
  getInv <- function () invmatrix
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve accepts the list created by makeCacheMatrix as an argument
## and returns the inverse of matrix from either the cache or a calculated
## inverse. It will populate the cache if empty
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  inv   
}
