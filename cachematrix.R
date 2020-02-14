## As indicated in the assignment, the two functions should compute the inverse of a
## matrix and cashe it for faster computations. I used the same logic as the example.


## This function creates the special matrix object that can cash its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  # Create an empty object 'm'
  set <- function(y) {  # Set the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x  # Get the value of the matrix
  setinv <- function(solve) m <<- solve  # Set the value of inverse matrix
  getinv <- function() m  # Get the value of inverse matrix
  list(set = set, get = get,  # Return the list of functions
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  m <- x$getinv()  # Check if the invese matrix has already been computed
  if(!is.null(m)) {  # If the object is not empty, cashed inverse matrix is returned
    message("getting cached data")
    return(m)
  }
  data <- x$get()  # If the object is empty, inverse matrix is computed and cashed
  m <- solve(data, ...) 
  x$setinv(m)
  m  # Return a matrix that is the inverse of x
}

## Try functions on a 5x5 matrix with randon whole numbers

mat <- matrix(sample(1:25, 25), 5, 5) # Create random matrix

mat_inv <- makeCacheMatrix(mat)  # Use first function
cacheSolve(mat_inv)  # Use second function (try it twice to cashe it first)