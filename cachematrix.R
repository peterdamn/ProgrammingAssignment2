## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix contain a list of four functions
# when matrix object is created, you have the matrix and the inv of it
# Although initially the inv is set to NULL
# the set functions are there for updates in case there are changes
# to the matrix object :)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## Write a short comment describing this function

# The function gives us the inverse of the matrix object.  
# If the inverse is calculated already, return the result and skip the
# computation (lazy evaluation). If not, it computes the inverse,
# use setinverse function to set the value.


# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

# the following test is from this post:
# https://class.coursera.org/rprog-011/forum/thread?thread_id=815#post-3616
#--------------------------------------------------------------------

# source('~/R coursera/ProgrammingAssignment2/cachematrix.R')
# exampleMatrix <- matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0), 3, 3)
# matrixVector <- makeCacheMatrix(exampleMatrix)

# matrixVector$get()

#[,1] [,2] [,3]
#[1,]    1    2    3
#[2,]    0    1    4
#[3,]    5    6    0

# cacheSolve(matrixVector)

#[],1] [,2] [,3]
#[1,]  -24   18    5
#[2,]   20  -15   -4
#[3,]   -5    4    1

#> cacheSolve(matrixVector)

#getting cached data.

#[,1] [,2] [,3]
#[1,]  -24   18    5
#[2,]   20  -15   -4
#[3,]   -5    4    1

#> matrixVector$get() %*% cacheSolve(matrixVector)
# get the cache and see if we get identity matrix with enough precision

#getting cached data.

#[,1]          [,2]          [,3]
#[1,]    1 -3.552714e-15 -8.881784e-16
#[2,]    0  1.000000e+00  0.000000e+00
#[3,]    0  0.000000e+00  1.000000e+00
