##T.Cecere
##Mar.19.15
## Take an invertible matrix and create a list of functions
## use "Global" assignment in parent environment (<<-) for matrix and eventual inverse

 
makeCacheMatrix <- function(x = matrix()) { 
     ##First time through, set everything to NULL
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     ##Create and return list of functions to be used by cacheSolve()
     get <- function() x
     setinverse <- function(solve) inv <<- solve
     getinverse <- function() inv
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
     
 } 
 

## Use list of functions from makeCacheMatrix
## Return a matrix that is the inverse of 'x' 


cacheSolve <- function(x, ...) { 
     ##Check to see if Inverse is already in memory, warn if not calculating
     inv <- x$getinverse()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     ##inv is null, take data from get(), calculate inverse, &
     ##put it into memory using setinverse(). Return answer.
     data <- x$get()
     inv <- solve(data, ...)
     x$setinverse(inv)
     inv
} 
