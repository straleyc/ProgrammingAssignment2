## makeCacheMatrix and cacheSolve together create a special matrix 
## which sets the value of the matrix, gets the value of the matrix 
## (checks it), inverts the matrix, and caches the inverted matrix 

## makeCacheMatrix creates the special matrix and returns a list of fxns that: 
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL  # m will store the cached inverse matrix
    
    # function to set the matrix
    set <- function(y) {
        x<<- y
        m <<- NULL
    }
    
    # fxn to get the matrix 
    get <- function() x
    
    # fxn to set the inverse 
    setinvmat <- function(invmat) m <<- invmat
    
    # fxn to get the inverse
    getinvmat  <- function() m
    
    # create list available to outside environments
    list(set = set, get = get, setinvmat = setinvmat, getinvmat = getinvmat)
}


## cacheSolve:  check to see if the inverse matrix has
## already been created.  If not, create is; if the 
## inverse exists, return it

cacheSolve <- function(x, ...) {
    
    ## put the value of the "getinvmat" subset of x into m
    m <- x$getinvmat()
    
    ## if m already exists, return message, "Getting cached data", and return m 
    if(!is.null(m)) {
        message("Getting cached data")
        return(m)
    }
    
    ## if m DOESN'T exist, create the inverse matrix
    data <- x$get()
    m <- solve(data, ...)
    x$setinvmat(m)
    
    ## return the inverted matrix 
    m
}
