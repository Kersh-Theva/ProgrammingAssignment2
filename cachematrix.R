## The functions below compute the inverse matrix (ie. solution) to a matrix and caches it 
## If the solution has already been computed, it will be retrieved from the cache instead of computing it again

## The function makeCacheMatrix creates a list that contains a matrix and its solution

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  
  #This function sets x to be the matrix y when the function is called, and resets the solution (ie. matrix inverse), s
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  #This function returns the matrix, x
  get <- function() x 
  
  #This function stores the matrix solution in setSolve
  setSolve <- function(solution) s <<- solution
  
  #This function returns the matrix solution
  getSolve <- function() solution
  
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## Return a matrix that is the inverse of the matrix 'x' (the solution), 
## but first checks to see if the solution has already been calculated

cacheSolve <- function(x, ...) {
  
  #Tries to find the solution for the matrix x, if it exists; if it does, the cached solution is returned  
  solution <- x$getSolve()
  if(!is.null(solution)) {
    message("getting cached data")
    return(s)
  }
  
  #if it does not, the matrix is obtained from the object created by makeCacheMatrix
  data <- x$get()
  
  #the solution is generated and stored 
  solution <- solve(data)
  x$setSolve(solution)
  
  #the solution is also returned 
  solution  
}
