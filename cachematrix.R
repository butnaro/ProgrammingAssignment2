## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# this function checks matrix to be invertible and if so stores the matrix and its solution, 
# the result of solve(), in cachce (a subsetted environvment)
makeCacheMatrix <- function(x = matrix()) {

inv <- NULL #setting the vector to be empty in the case that the CachSolve was not called

  
  set_matrix <- function(y){  # setting up the matrix itself
    x <<- y # storing the matrix in Cache, in this environment the CacheSolve can go through this and
            #if it is was changed
    inv <<- NULL
  }
  
  get_matrix <- function () x
  setinv <-function (solve) inv <<- solve
  getinv <- function () inv 
  list (set_matrix=set_matrix, get_matrix=get_matrix, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function
# this function checks two things, rather the matrix was solved before and is indeed invertible, and 
#raher it was not changed - if both are TRUE than it returns the result of solve() for the matrix stored in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #comparison betweeen matrixes
  inv <- x$getinv() # retrieves the inverse if already calculated
  
  if (!is.null(inv)){#testing rather cahceSolve had been runned before
    if (x$set_matrix()==x$get_matrix()){ #subset to that condition, checking rather it is unchanged
      message ("getting cached data")
      return (inv)
    }
  }
  
  data <- x$get_matrix()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}

