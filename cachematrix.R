## Function creates a list of 4 methods to be used as a parameter for the cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
    
    #First we set m to be null to restart the process to be used later
    m <- NULL
    
    #This function is used in future cases to avoid having to reassign the function list to another variable
    #Values are assigned in the parent environment via <<- operator
    set <- function(y){
      x <<- y
      m <<- NULL
    }
    
    #Simply returns the matrix - (2)
    get <- function() x
    
    #Assign the argument to m and makes this available in the parent environment - (3)
    setmatrix <- function(solve) m <<- solve
    
    #Returns the value for m - (1) 
    getmatrix <-function() m
    
    #List of defined functions defined above
    list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)

}


#Function takes makeCacheMatrix function that contains the list of defined functions
cacheSolve <- function(x, ...) {
  
  #First m is set to the existing value - (1)
  #from above 1 simply returns the value of m
  m <- x$getmatrix()
  
  #If m is not null then its value is currently stored and is returned 
  if (!is.null(m)) {
    
    message("getting cached data")
    
    return(m)
  }
  
  #Set the matrix to calculate a new solution/solve - (2)
  #2 returns the value of x     
  matrix <- x$get()
  
  #Calculate a new solution/solve based on the matrix
  m <- solve(matrix, ...)
  
  #Set this new value for the solution/solve to m - (3) 
  #m is the new calculated solution/solve, it takes this value and sets it to m
  x$setmatrix(m)
  
  #Displays the value of m
  m
}
