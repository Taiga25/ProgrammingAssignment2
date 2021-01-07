## This assignment is about solving the inverse of a matrix by caching the 
# result within a lexical scope of a function:  "makeCacheMatrix" and 
# "cacheSolve".
## I assigned the input x as a matrix
## and then set the solved value "s" as a null.
## I also changed every reference to "mean" to "solve".
## The function "makeCacheMatrix" creates a new, unique environment.
# It creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
## The function "cacheSolve" returns the inverse of the matrix that is 
# returned by makeCacheMatrix function.
## Same here, changed "mean" to "solve" and "m" to "s"
cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}