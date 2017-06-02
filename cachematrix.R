# Matrix inversion is usually a costly computation and there may be some 
# benefit to caching the inverse of a matrix rather than compute it repeatedly

# Computing the inverse of a square matrix can be done with the solve function in R. 
# For example, if X is a square invertible matrix, then solve(X) returns its inverse.

# makeCacheMatrix: This function creates a list that is a special "matrix" object that can cache its inverse.
# The list contains a function to: 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

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


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
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

## Sample run data
## > x = rbind(c(1, 2), c(3, 4))
## Print and solve for the inverse of the matrix without using the function
## > print(x)
##      [,1] [,2]
## [1,]    1    2
## [2,]    3    4
## > solve(x)
##      [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5

## Use the function on the matrix
## > m = makeCacheMatrix(x)
## > m$get()
##      [,1] [,2]
## [1,]    1    2
## [2,]    3    4

## Use the second fuction to solve for the inverse (Cache does not exist)
## > cacheSolve(m)
##      [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5

## Run cacheSolve again. Inverse is retrieved from cache created on first run.
## > cacheSolve(m)
## getting cached data.
##      [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5
