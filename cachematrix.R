## This function, makeCacheMatrix, creates a special matrix.
## In Fact the function creatye a "list"  which containing a function to
## 1- set the value of the matrix
## 2- get the value of the matrix
## 3- set the value of the inverse of this matrix
## 4- get the value of this inverse

## Function to create the matrix 

makeCacheMatrix <- function(x = matrix()) {  # coerce for sure that x is a matrix
  
  m <- NULL
  
  set <- function(y){  # y is a free variable for the first function
    
    x <<- y
    
    m <<- NULL
  }
  get <- function() x
  
  setinverse <- function(solve) m <<- solve ## Anonymous function to calcul the Inverse
  
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function read the inverse of a matrix

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x = matrix(), ...) {
  
  m <- x$getinverse()
  
  if(!is.null(m)){             # test to see if the matrix already exists
    
    message("getting cached data")
    
    return(m)
  }
  my_matrix <- x$get
  
  m <- solve(my_matrix, ...)
  
  x$setinverse(m)
  
  m
}