#Writing a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its own inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #Setting the inverse to a null value, yet to be computed
  s <- function(z) {
    x <<- z ##Storing the x matrix outside of the current environment
    inv <<- NULL #everytime there's a change, inv becomes null
  }
  g <- function() {x} ##Returns the x matrix
  ssolve <- function(solve) {inv <<- solve} ##Storing the inverse outside of the current environment
  gsolve <- function() {inv} ##Returns the inv that has been computed
  list(s = s, g = g,
       ssolve = ssolve,
       gsolve = gsolve)
}


##  This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated,
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  inv <- x$gsolve() #get the inverse from list
  if(!is.null(inv)) {
    message("retrieving inverse")
    return(inv) #inverse returned if not null
  }
  data <- x$g() #Otherwise the called matrix is retrieved from the list
  i <- solve(data, ...) #calculate the inverse
  x$ssolve(inv) #cache the inverse in the list
  inv #inverse matrix of x returned to the user
}


#Testing the code
B <- matrix(c(1,2,3,4),2,2)
solve(B)