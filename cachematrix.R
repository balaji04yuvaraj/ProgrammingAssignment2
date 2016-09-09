## Put comments here that give an overall description of what your
## functions do

#This assigenment is to write a pair of functions that cache the inverse of a matrix.
#Objective is, caching the inverse of a matrix rather than compute it repeatedly 

## Write a short comment describing this function
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#creating a list of functions set(), get(), setinv(),getinv()  .


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function

#cacheSolve: This function computes the inverse of the special "matrix"
#returned by makeCacheMatrix above. If the inverse has already been calculated
#(and the matrix has not changed), then the cacheSolve should retrieve the
#inverse from the cache.

# Inverse of a square matrix is done with solve function.


#NOTE:The Value of x is:
#   [,1] [,2]
#[1,]    2    3
#[2,]    2    2

#Output
#> cacheSolve(sqmat)  # (NOTE: sqmat <- makeCacheMatrix())
#     [,1] [,2]
#[1,]   -1  1.5
#[2,]    1 -1.0


cacheSolve <- function(x, ...) {
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


