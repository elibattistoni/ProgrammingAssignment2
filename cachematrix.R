######################################################################################
## makeCacheMatrix is a function that creates a special matrix object to be used by 
## downstream R code
makeCacheMatrix <- function(x = matrix()) {# x is an empty matrix by default
    
    # initialize an object with null value for the inverse matrix to be created later
    inv_mat <- NULL 
    
    # set the value of the matrix and reset the inverse 
    set <- function(y) {
        x <<- y # the matrix x (in the parent environment) is assigned the value y
        inv_mat <<- NULL # reset to null the value of the inverse matrix inv_mat (in the parent environment)
        # this clears any value of inv_mat that was cached by a prior execution of cacheSolve()
    }
    
    # get the matrix x
    get <- function() x # since x is not defined within get(), R retrieves its value
    # from the parent environment of makeCacheMatrix()
    
    # set the inverse matrix
    setinverse <- function(solve) inv_mat <<- solve # assign the input to inv_mat in the parent environment
    
    # get the inverse
    getinverse <- function() inv_mat
    
    # create an object with a list in which each element is a function
    # and return it to the parent environment
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

######################################################################################
## the cacheSolve() function creates the inverse of the special matrix defined with 
## makeCacheMatrix()
cacheSolve <- function(x, ...) {
    # retrieve an inverse matrix from the object passed in as the argument
    inv_mat <- x$getinverse()
    
    # now check if the result is NULL; if not, a valid cached inverse matrix exists
    # and we can return it to the parent environment
    if (!is.null(inv_mat)) {
        message("getting cached data")
        return(inv_mat)
    }
    
    # if !is.null(inv_mat) is FALSE
    data <- x$get() # get the matrix from the input object
    inv_mat <- solve(data, ...) # calculate the inverse matrix
    x$setinverse(inv_mat) # set the inverse matrix
    inv_mat # return the inverse matrix to the parent environment by printing it
}