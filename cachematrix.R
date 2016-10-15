## Put comments here that give an overall description of what your
## functions do

## Write the following functions:
##`makeCacheMatrix`: This function creates a special "matrix" object

makeCacheMatrix <- function(x = matrix()) {
    
    # set makeCacheMatix's m variable equal to nothing. 
    m <- NULL             
   
    # create a function called set() and feed it a variable called y. 
    set <- function(y) {  
      # within the set() function, set the local variable x equal to y.
      x <<- y              
      # Within the set() funciton, set the local variable m equal to nothing. 
      m <<- NULL          
    }
    
    #  another function, this one doing nothing by returning the matrix which is being solved for.
    get <- function() x   
    
    # Setting global m and setsolve to the inverse of the matrix being solved for
    setsolve <- function(solve) m <<- solve(x)
    
    # another function, this one setting itself the m, which was just set to the inverse of the matrix
    getsolve <- function() m   
    
    # Generating an unname list within the makeCacheMatrix environment which contains the summary 

    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)  
    
}

# Build a matrix, then call the function
A <- matrix(c(1,2,3,0,5,6,7,8,9), nrow = 3, ncol = 3)
makeCacheMatrix(A)

B <- matrix(c(1,2,3,0,0,6,7,8,9), nrow = 3, ncol = 3)
makeCacheMatrix(B)

# Second requiement: Write the following function
# `cacheSolve`: This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.
# If not, it calculates the value and adds it to the cache. 

cacheSolve <- function(x, ...) {

    # Check the cache, assigning cachemean variable m the value in 
    m <- x$getsolve()

    ## should check to see if the inverse has been calculated.
    # if anything is returned within value m (therefore, x$getsolve)
    
    if(!is.null(m)) {
      
      ## If it has, it returns the cached value.
      message("getting cached data")
      
      ## exit cachemean
      return(m)
  }
  
  # Assign a local variable, data the cache's x$get value
  data <- x$get()
  
  # Usign the solve() function, calculate the inverse of the matrix
  m <- solve(data, ...)
  
  # Update the value of the cache. 
  x$setsolve(m)
  
  m
}

# test the function.
cacheSolve(A)
