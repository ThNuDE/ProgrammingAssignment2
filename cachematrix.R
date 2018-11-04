## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## First: Set the Inverse Matrix 'inverseMatrix' to NULL
## Second: Set the value of the matrix 'funtion set'
## Third: Get the value of the matrix 'function get'
## Fourth: Sets the inverse matrix
## Fifth: Gets the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
       
	   inverseMatrix <- NULL 
       
	   set <- function(y) {
              x <<- y
              inverseMatrix <<- NULL
       }
       
	   get <- function() {x}
       setsolve <- function(solve) {inverseMatrix <<- solve}
       getsolve <- function() {inverseMatrix}
       list(set = set, get = get,
            setsolve = setsolve,
            getsolve = getsolve)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

       inverseMatrix <- x$getsolve() 
       if(!is.null(inverseMatrix)) {
              message("retrieving inverse")
              return(inverseMatrix)
       }
       
	   data <- x$get() 
       inverseMatrix <- solve(data, ...)
       x$setsolve(inverseMatrix) 
       inverseMatrix
}
