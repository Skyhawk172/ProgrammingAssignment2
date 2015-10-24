######################### 
# Contains two functions that together cache a matrix and calculate its
# inverse, unless the inverse has already been calculated, in which case
# the functions simply return the result.
#########################

#########################
#  This function defines an object list comprised of simple functions used  
#  for getting and setting the inverse of a input matrix. The variables
#  are defined as global (<<-) and are accessible from everywhere.
#  INPUT: matrix
#  RETURN: "matrix" object containing four functions
#########################
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvmat <- function(invmat) m <<- invmat
  getinvmat <- function() m
  list(set = set, get = get,
       setinv = setinvmat,
       getinv = getinvmat)
}

#########################
# This function checks if the inverse of an input matrix object has already 
# been calculated. If so, then it simply returns the cached inverse matrix. 
# If not, it calculates the inverse matrix and caches it.
# INPUT: matrix object
# RETURN: inverse matrix
#########################
cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
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
