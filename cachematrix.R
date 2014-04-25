## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The function takes a matrix x as an input
# it define the following in its scope:
# variable "inv" intead to hold the inverse
# four functions:
#   1- set
#   2- get
#   3- setinv
#   4- getinv
#
# The set function uses the <<- operator to assign the values defined at the scope of makeCacheMatrix environment level.
# the following three functions have x and inv as free variables that aren't defined within thier scopes, 
# so they search on the parent level - which is makeCacheMatrix level - in which they are defined. 
# accordingly they are able to set the variables using the <<- operator.
# 
# the get points to a function that returns the matrix
# the getinv points to a function that returns the inv of the matrix.
# 
# the function returns a list that holds the four functions

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverted_matrix) inv <<- inverted_matrix
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function
# the function takes an argument a list that is returned from the makeCacheMatrix
# the function checks first if the an inv matrix has been computed before or not
# that happens by checking if the returned value from the getinv() function is null or not?
#
# if the return isn't null this mean that an inverse have been computed before, so a message is prompted with this meaning
# and the function return with the inverted matrix.
#
# if the return value is null - that means that the inverse was never computed on this object before.
# So, the matrix is first returned using the get() function
# Then a reverse for the matrix is computed using the solve function.
# Finally, the inverted matrix if feedback to the makeCacheMatrix list using the function setinv(inv)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  
}
