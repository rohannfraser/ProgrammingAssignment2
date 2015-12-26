# Calculating the inverse of a matrix can be a resource intense operation.
# There can therefore be a benefit to caching the inverse of a matrix 
# rather than compute it repeatedly. 
# The following two functions can used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) inv <<- inverse
     getinverse <- function() inv
     list(set=set,               get=get, 
          setinverse=setinverse, getinverse=getinverse)
}

# The function below returns the inverse of the matrix. It first checks if
# the inverse has already been computed and if that is the case,
# it gets the result and skips the computation. 
# If the inverse has not been computed yet it computes the inverse and
# sets the value in the cache via the setinverse function.

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

## Sample runs to show the results are as expected:
#  x = matrix(1:4, nrow=2, ncol=2)
#  m = makeCacheMatrix(x)
#  > m$get()
#       [,1] [,2]
#  [1,]    1    3
#  [2,]    2    4

# After the first run there is no cache:
# > cacheSolve(m)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# After the second run the inverse has already been calculated and 
# the message "getting cached data." is displayed.
# > cacheSolve(m)
# getting cached data.
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

#Check if original matrix times inverse matrix gives identity matrix
# > m$get() %*% m$getinverse()
#      [,1] [,2]
# [1,]    1    0
# [2,]    0    1