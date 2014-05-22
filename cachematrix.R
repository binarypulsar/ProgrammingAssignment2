## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object, which is really a list
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it gets the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of the
## data and sets the value of the inverse in the cache via the setinverse
## function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


#Test Cases
# amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
# amatrix$get()         # Returns original matrix
# cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
# amatrix$getinverse()  # Returns matrix inverse
# cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse

# amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
# cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
# amatrix$get()         # Returns matrix
# amatrix$getinverse()  # Returns matrix inverse

## Usage:
## 1. Pass a matrix and cache it by passing it to the makeCacheMatrix function
## > myCachedMatrix <- makeCacheMatrix(matrix(c(0, -5, -4, 0), nrow=2, ncol=2, byrow=TRUE))
## 2. Verify that the matrix X is available in the myCachedMatrix environment
##    but not in the global environment:
## > myCachedMatrix$getMatrix()         => you will see a the matrix passed in 1.
## > X                                  => you will get Error object 'X' not found, or something else
## 4. Verify that the inverse of X is not yet calculated
## > myCachedMatrix$getInverseMatrix()  => you will get NULL
##  Calculate and get its inverse with the cacheSolve function
## >  cacheSolve(myCachedMatrix)        => you will get the inverse matrix of X
## 5. Verify the content of the cache
## > myCachedMatrix$getInverseMatrix()  => you will get the very same inverse matrix of X
## 6. Verify that the matrix product of X and its inverse returns the identity matrix
## > myCachedMatrix$getMatrix() %*% myCachedMatrix$getInverseMatrix()
## 7. Invoke again cacheSolve to verify Inverse matrix is coming from the cache
##    and it is not calculated
## > cacheSolve(myCachedMatrix)         => you will get the very same inverse matrix of X
## 8. Set a new matrix and verify the cache for the inverse matrix has been cleared.
## > myCachedMatrix$setMatrix(matrix(c(0, -3, -2, 0), nrow=2, ncol=2, byrow=TRUE))
## > myCachedMatrix$getMatrix()         => you will get the new matrix
## > myCachedMatrix$getInverseMatrix()  => you will get NULL
## > cacheSolve(myCachedMatrix)         => you will get another inverse matrix, because the matrix has changed

# Maybe trying with a bigger invertible matrix you can notice if you are using cache or not depending of the execution time:
# x <- 2000
# m <- matrix(runif(x^2),x)
# For my laptop, 1000x1000 was so fast, 10000x10000 so slow,
# and 2000x2000 just 10 seconds, then with cache less than 1 second,

# Here is an example where the inverse was not previously in the cache
# p = matrix(11:14,2,2  )  
# pp<-makeCacheMatrix(p)
# cacheSolve(pp)
# [,1] [,2]
# [1,]   -7  6.5
# [2,]    6 -5.5

# Here i store the data in the cache first
# s<-matrix(21:24,2 , 2)
# > ss<-makeCacheMatrix(s)
# > ss$setinverse(solve(s)) #here i am storing the inverse in the cache
# > ss$getinverse() # here i am just getting it to see if the inverse is there for sure
# [,1]  [,2]
# [1,]  -12  11.5
# [2,]   11 -10.5
# > cacheSolve(ss) # here i find the inverse but the data is already in the cache
# getting cached data
# [,1]  [,2]
# [1,]  -12  11.5
# [2,]   11 -10.5