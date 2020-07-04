## This function will calculate and store in cache the inverse
## of a matrix if it hasn't changed

## This function creates a special "matrix" (it's really a list
## with 4 functions) that can store in cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv_ma <- NULL
  set <- function(y) {
    x <<- y
    inv_ma <<- NULL
  }
  get <- function() {x}
  setinverse <- function(inver) {inv_ma <<- inver}
  getinverse <- function() {inv_ma}
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of a matrix or
## get that inverse if it was calculated before. If the last is the
## case, the function will return the inverse in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_ma <- x$getinverse()
  if(!is.null(inv_ma)) {
    message("getting cached matrix")
    return(inv_ma)
  }
  matriz <- x$get()
  inv_ma <- solve(matriz, ...)
  x$setinverse(inv_ma)
  inv_ma
  
}

## Example:

matr<-makeCacheMatrix(diag(c(1,2,3))) #Store the matrix
matr$get()                            #Get the stored matrix
matr$getinverse()                     #Is NULL (the inverse hasn't be calculated)
cacheSolve(matr)                      #Calculate the inverser and store it
cacheSolve(matr)                      #This doesn't calculate yhe inverse because it was stored
