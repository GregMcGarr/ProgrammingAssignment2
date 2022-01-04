## This assignment involves writing a pair of functions that cache the inverse of a matrix.

# since matrix inversion is often computationally expensive, it may be beneficial to cache it rather than repeatedly calculate it.
# we need to assume that the matrix supplied to the function is always invertible.


# FUNCTION 1: -------------------------------------------------------------

# This function creates a special "matrix" object that can cache its inverse. 

  # This function is really a list containing a function to:

  # 1.  set the value of the matrix

  # 2.  get the value of the matrix

  # 3.  set the value of the inverse

  # 4.  get the value of the inverse 


makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL # initialized as an object to be used by set() later on
  
  set <- function(y) {
    x <<- y    # <<- assigns the value on the right side of the operator (y) to an object in the parent environment named by the object on the left side of the operator (x)
    inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) inv <<- inverse
  get_inverse <- function() inv
  list(set = set, 
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


# FUNCTION 2: -------------------------------------------------------------

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  dat <- x$get()
  inv <- solve(dat, ...) # solve() computes the inverse of a square matrix
  x$set_inverse(inv)
  inv
}


# Test the functions: ---------------------------------------------------------

m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)

m1

solve(m1) # returns the inverse of m1 after computation

my_matrix <- makeCacheMatrix(m1)

cacheSolve(my_matrix) # returns the inverse of m1 after computation (same as using solve(m1) above)

cacheSolve(my_matrix)# running this again returns the inverse of m1 from the cache
