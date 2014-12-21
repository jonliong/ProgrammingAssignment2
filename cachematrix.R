## The functions 
## functions do 

## This function creates a special "matrix" object that can cache its inverse 
makeCacheMatrix <- function(x = matrix()) { 

  m <- NULL ## Initialize the storage variable for the inverse matrix
  y <- NULL ## Initialize the storage variable for the input matrix
  
  getmatrix <- function() { ## Method that retrieves the input matrix
    x
  }
  
  setmatrix <- function(y) { ## Method that sets the input matrix from the user
    x <<- y ## Sets the input matrix to 
    m <<- NULL ## Reinitializes the inverse matrix for every input that is 
               ## diffrent fro the last
    
  }
  
  getinverse <- function() { ## Method that retrieves the calculated inverse matrix
    m
  }
  
  setinverse <- function(inverse) { ## Method that sets the inverse matrix
    m <<- inverse
  }
  
  ##
  list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, 
       getinverse = getinverse)
} 
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse from the 
## cache 
cacheSolve <- function(x=matrix(), ...) { 
  m <- x$getinverse() ## Retrieve the cached inverse matrix, if any
  aa <- x$getmatrix() ## Retrieve the input matrix
  if (!is.null(aa)) { ## Check if the input matrix is NULL
    temp <- solve(aa) ## If input is not NULL, solve for its inverse
    ## Check if the inverse of the input is the same the the cached inverse
    if (identical(m, temp)) {  
      print("Getting from cache...")
      return(m) ## Return cached inverse if input is the same as the previous one
    }
  }
  
  x$setmatrix(aa) ## Set the input matrix back to the cache for comparison the next time
  m <- solve(aa) ## Calculate the inverse of the input
  x$setinverse(m) ## Cache the solution
  
  m
  ## Return a matrix that is the inverse of 'x' 
} 
