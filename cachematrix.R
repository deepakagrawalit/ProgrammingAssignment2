## Script contains 2 methods . "makeCacheMatrix" is to create special "matrix"
## object and "cacheSolve" to check whether cache is there or not and if it not 
## there and same is method is calcullating and settign back to object

## creating special "matrix" to cache its inverse for futher use
makeCacheMatrix<- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
      	x <<- y
            inverse <<- NULL
	}
      get <- function() x
      setInverse <- function(inve) inverse  <<- inve
      getInverse <- function() inverse 
      list(set = set, get = get,
           setInverse = setInverse ,
           getInverse = getInverse)
}

## cache resolver method that checks on given object whether inverse is 
## available then get from cahce or caculate inverse and set it back to 
## object for further use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  inverse  <- x$getInverse()
        if(!is.null(inverse)) {
                print("getting cached data")
                return(inverse)
        } 
        data <- x$get()
        inverse  <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}
