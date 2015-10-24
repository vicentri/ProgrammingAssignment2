## created: 22.10.2015 RVT
## The main purpose of this function is to test lexical scoping and cache functions 
## in order to reuse existing results instead of recomputing it. 
 
## Create a special object that stores a numeric vector and cache's its mean.

makeCacheMatrix <- function(x = matrix(), ...) {
  m <- NULL
  set <- function(y){
  	x <<- y
  	m <<- NULL
}        
  get  <- function() x 					# Get x vector
  setinv <- function(solve) m <<- solve   	# Sets the inverse
  getinv <- function() m 				# get inverse, m
  list(set = set, get = get, setinv = setinv, getinv = getinv) # Gets the vector
}

## Return the inverse of the matrix, first check if 
## the inverse is available (has been previously computed)

cacheSolve <- function(x, ...) {
  m <- x$getinv()        
  if(!is.null(m)){ 
      return(m)
    } else {
 	minv <- solve(x$get())
  	## m <- solve(minv)
  	x$setinv(minv)
  	return(minv)
	}
}

## Test:
## Set the vector and populate it
## a <- makeCacheMatrix()
## a$set(matrix(1:10,5,5))
## cacheSolve(a) # get cache