## Compute the inverse of a square matrix.
## If the inverse matrix is already cached, retrieve it instead of computing again.
## If not, create cache of the inverse matrix.
## The original matrix is changed using the "set" function in makeCacheMatrix.


## Create a special "matrix" object that can cache its inverse in its own environment.
## Argument is a square matrix.
makeCacheMatrix <- function(x = matrix()) {
    ## Set default 'inverse' in this parent environment of the below functions so R does not
    ## search other environments for values of 'inverse'.
    inverse <- NULL
    ## Change and set the value of the original matrix.
    ## Clear previous cached inverse so it can be recomputed.
    ## The '<<-' assignment modifies the existing variable found by going up parent evironments.
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    ## Get the value of the original matrix
    get <- function() x
    ## Set the value of the inverse
    setinverse <- function(solve) inverse <<- solve
    ## Get the value of the inverse
    getinverse <- function() inverse
    ## Return a special "matrix" with included functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Compute the inverse of the special "matrix" object that was created in the makeCacheMatrix function.
## If a cache of the inverse already exists, retrieve it and save valuable computation time.
cacheSolve <- function(x) {
    ## Retrieve inverse value that exists in special "matrix" created in makeCacheMatrix function
    inverse <- x$getinverse()
    ## If cached inverse exists, return this matrix.
    ## For a changed matrix, a new correct inverse matrix will be computed since the "set" function
    ## in makeCacheMatrix erases the previous cached inverse.
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    ## Retrieve original matrix created in makeCacheMatrix function
    data <- x$get()
    ## Compute inverse using the 'solve' function
    inverse <- solve(data)
    ## Set inverse to special "matrix" created in makeCacheMatrix function
    x$setinverse(inverse)
    ## Return a matrix that is the inverse of 'x'
    inverse
}
