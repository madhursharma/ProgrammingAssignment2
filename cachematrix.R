## Put comments here that give an overall description of what your
## functions do

## Creates a special matrix object that can cache its universe objects

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) s <<- solve
  getInv <- function() s
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)

}


## Computes inverse of matrix and caches the results if inverse has been computed previously


cacheSolve <- function(x, ...) {
        inv_mat <- x$getInv()
        
        if(!is.null(inv_mat)) {
          message("getting cached data")
          return(inv_mat)
        }
        #Check if matrix is N x N
        ## Return a matrix that is the inverse of 'x'
        data <- x$get()
        size = dim(data)
        if(size[1] == size[2]) {
          #Check if matrix is in cache
          
          inv_mat <-solve(data, ...)
          x$setInv(inv_mat)
        } else {
          x$setInv(NA)
          inv_mat <- NA
        }
        inv_mat
}
