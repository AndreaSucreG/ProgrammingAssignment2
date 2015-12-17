## The following functions correspond to the Assignment 2 from R Programming of Coursera. 
## The functions makeCacheMatrix and cacheSolve take advantage of the scoping rules of the R language making it possible to cache inverse of a matrix rather than compute it repeatedly 

## The first function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.
## The function takes the matrix x as an argument with the default value of an empty matrix and returns a list that includes:
##    a) A function to set the value of the matrix
##    b) A function to get the value of the matrix
##    c) A function to set the value of the inverse matrix
##    d) A function to get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  ## Set the value of the matrix
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  ## Get the value of the matrix
  get <- function() x
  ## Set the value of the inverse matrix
  setinverse <- function(inverseX) inverseMatrix <<- inverseX
  ## Get the value of the inverse matrix
  getinverse <- function() inverseMatrix
  ## List of functions described above (a, b, c, d)
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The second function "cacheSolve" computes the inverse of the special "matrix" returned by "makeCacheMatrix". 
## If the inverse has already been calculated (and the matrix has not changed), then the "cacheSolve" should retrieve the inverse from the cache.
## The function takes the matrix x as an argument and returns the inverse of x. 


cacheSolve <- function(x, ...) {
  ## Store in "inverseMatrix" the value of the inverse of matrix x
  inverseMatrix <- x$getinverse()
  ## If the value of "inverseMatrix" is not NULL, then return the cache value of "inverseMatrix" that was stored previously and exit function cacheSolve
  if(!is.null(inverseMatrix)){
    print("Getting cached data")
    return(inverseMatrix)
  } 
  ## If the value of "inverseMatrix" is NULL, then continue
  
  ## Store in "data" the value of matrix x
  data <- x$get()
  ## Store in "inverseMatrix" the result of calculating the inverse of matrix x with solve() function
  inverseMatrix <- solve(data,...)
  ## Set the value of inverse of matrix x as "inverseMatrix"
  x$setinverse(inverseMatrix)
  ## Return "inverseMatrix" and exit the function cacheSolve
  inverseMatrix
  
}
