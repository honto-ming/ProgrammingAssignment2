## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL 
    ## a "re-constructor" where you erase the inverse and set the new 
    ## values of x
    set <- function(y) {   
        x <<- y 
        m <<- NULL
    }
    ## returns the original matrix
    get <- function() x
    # set the inverse stored in this function
    setInverse <- function(inv) {inverse <<- inv}
    # returns the inverse stored in this function
    getInverse <- function() inverse
    ## This is what the function makeCacheMatrix returns when called. So you can
    ## use this vector as an "object" by making "calls" to the list elements
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # check to see if the makeVector "object," x, already has the mean cached
        inv <- x$getInverse()
        if(!is.null(inv)) {
            # already cached. No need to perform calculations
            message("getting cached data")
            return(inv)
        }
        # Need to perform calculations
        data <- x$get()
        inv <- solve(data)
        # set the mean value in makeVector "object"
        x$setInverse(inv)
        inv
}
