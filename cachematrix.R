## makeCacheMatrix has a default argument and returns a list of 4 functions.
## It is used to manage the matrix of which the function cacheSolve computes the inverse
## set -> store the matrix in a global variable x
## get -> return x
## setinverse -> cache the value of the inverse when available 
## getinverse -> return the computed value of the inverse


makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {    
     x <<- y
     inv <<- NULL
  }
     get <- function() x
     setinverse <- function(inverse) inv <<- inverse
     getinverse <- function() inv
     list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes as argument a list of 4 functions acting on a matrix
## check if the inverse matrix already exists 
## if yes retrieve it from the cache (avoiding a double computation!)
## if not, get the original matrix, compute it and store it into "inv"
## return inv

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
     inv <- x$getinverse()
       if(!is.null(inv)) {
         message("getting cached data")
         return(inv)
          }
       data <- x$get()
       inv <- solve(data, ...)
       x$setinverse(inv)
       inv
}
