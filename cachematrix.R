## makeCacheMatrix : this function gets a matrix as an input, set the value of the matrix,
#get the value of the matrix, set the inverse Matrix and get the inverse Matrix. 
#The matrix object can cache its own object. 


  makeCacheMatrix <- function(x = matrix())  {
    inv <- NULL
      set <- function(y){
      x <<- y
      inv <<- NULL
  }
    get <- function() x                                                   ##get the value of the matrix
    setInverse <- function(solveMatrix) 
    inv <<- solveMatrix                                                   ##set the value of the invertible matrix
    getInverse <- function() inv                                          ##get the value of the invertible matrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  }

  
  
## cacheSolve: this function gives the output of the previous function makeCacheMatrix(matrix) as an input and checks 
# if it has any value in it or not.
# In case inverse matrix from makeCacheMatrix((matrix) is empty, it gets the original matrix data from 
# and set the invertible  matrix by using the solve function.
# In case inverse matrix from makeCacheMatrix((matrix) has some value in it (always works
#after running the code 1st time), it returns a message  "Getting Cached Invertible Matrix" 
#and the cached object



 cacheSolve <- function(x, ...)
   {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
      if(!is.null(inv)){
          message("getting cached inverse matrix")
          return(inv)
          }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv      
  }

 
 