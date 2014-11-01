makecacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinversematrix <- function(inverse) i <<- inverse
  getinversematrix <- function() i
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}
cacheSolve <- function(x, ...) {
  i<-x$getinversematrix()
  if(!is.null(i)){
    message("getting cached matrix")
    return(i)
  }
  matrix<-x$get()
  i<-solve(matrix, ...)
  x$setinveresematrix(i)
  i
}