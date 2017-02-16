## These two functions together should cache the inverse of a "matrix" object.

##makeCacheMatrix: This function creates a special “matrix” object that can cache its inverse.
## This function sets and gets the value of the matrix as well as setting then getting the value of the inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
          x <<- y
          inv <<- NULL
        }
        get <- function() x
        setInverse <- function(solveMatrix) inv <<- solveMatrix
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special “matrix” returned by makeCacheMatrix above.
## This function checks to see if the inverse has been calculated. If so, it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv      
}
