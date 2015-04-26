#KaelisPrae
#tglick

#Assignment 2: Caching the Inverse of a Matrix
#The functions cache the inverse of a matrix

#makeCacheMatrix creates a list containing a function to
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of inverse of the matrix
# 4. Get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}

# The following Function reutnrs the inverse of a matrix
# It first checks if the inverse already exists in the cache
# If yes, it displays "getting cached data" and displays answer
      # without additional calculations
# In no, it calculates the inverse of a matrix and puts in 
      # in the catch for the future

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
}

# Test Fuction
# x <- makeCacheMatrix(matrix(c(6,2,3,6), nrow=2, ncol=2))
# cacheSolve(x)
# 
# cacheSolve(x)

# Result
# > x <- makeCacheMatrix(matrix(c(6,2,3,6), nrow=2, ncol=2))
# > cacheSolve(x)
# [,1] [,2]
# [1,]  0.20000000 -0.1
# [2,] -0.06666667  0.2
# > cacheSolve(x)
# getting cached data
# [,1] [,2]
# [1,]  0.20000000 -0.1
# [2,] -0.06666667  0.2

# Test Fuction

x <- makeCacheMatrix(matrix(c(6,2,3,6), nrow=2, ncol=2))
cacheSolve(x)

cacheSolve(x)

