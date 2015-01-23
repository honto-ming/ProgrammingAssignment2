## This is for Assignment 2 for Coursera Programming in R. These functions below
## can be used to calculate the inverse of a matrix. The result is cached after 
## the first time the inverse calculation is made for the matrix so that the 
## expensive inverse calculation does not have to be repeated every time.

## This creates the special "object" that stores the matrix, the inverse, and 
## the functions to retrieve them 
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


## This function gets the inverse of the matrix stored in x. It tests if the
## inverse is already cached. If not, it performs the inverse calculation. If it
## is already in cached it just retrieves and returns it.
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
