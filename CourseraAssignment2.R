## This function, makeCacheMatrix, creates a special matrix.
## In Fact the function creatye a "list"  which containing a function to
## 1- set the value of the matrix
## 2- get the value of the matrix
## 3- set the value of the inverse of this matrix
## 4- get the value of this inverse

## Function to create the matrix 

makeCacheMatrix <- function(x = matrix()) {
  
              m <- NULL
              
              set <- function(y){
                
                x <<- y
                
                m <<- NULL
              }
              get <- function() x
              
              setsolve <- function(solve) m <<- solve ## Anonymous function to calcul the Inverse
              
              getsolve <- function() m
              
              list(set = set, get = get,
                   setsolve = setsolve,
                   getsolve = getsolve)
              
}
  





## This function read the inverse of a matrix

## Return a matrix that is the inverse of 'x'

  cacheSolve <- function(x = matrix(), ...) {
    
    m <- x$getsolve()
    
    if(!is.null(m)){
      
      message("getting cached data")
      
      return(m)
    }
    mymatrix <- x$get
    
    m <- solve(mymatrix, ...)
    
    x$setsolve(m)
    
    m
  }


