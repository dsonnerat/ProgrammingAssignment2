## This function create a list of functions that allows to set and get a matrix and an inverse matrix. 
## Matrix and inverse matrix are stored in the function environment.
## The function has a optional argument witch is the matrix.
## The availables functions contained in the list are:
## set: set the matrix value
## get: get the matrix value
## setinverse: set the inverse value.
## getinverse: get the inverse value.
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL #The memorized inverse value 
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function(){
                x
        }
        setinverse <- function(inv){
                inverse <<- inv
        }
        getinverse <- function(){
                inverse 
        }
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function calculate the inverse of the cache matrix and returns it.
## Cache matrix argument has to be obtained using makeCacheMatrix function.
## Optional arguments ... will be passed to the solve function.
## When this function is called many times for the same cahe matrix object, 
## the inverse calculation is done only once (the fist time).
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached inverse")
                return(inverse)
        }
        message("calculating inverse")
        data <- x$get()
        identity  <- diag(dim(data)[1]) ## indentiy matrix that have the same size than the matrix to inverse.
        inverse <- solve(data,identity, ...) ## identity matrix is used in order to pass ... arguments in a proper maner.
        x$setinverse(inverse)
        inverse
}
