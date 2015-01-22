## This cachematrix.R library consists of two functions.  These
## functions serve to take a matrix, find the matrix's inverse,
## then cache the inverse matrix so that it doesn't need to be
## calculated each time it needs to be referenced.


## makeCacheMatrix method takes in a matrix and returns a list
## of getter and setter methods for caching the matrix's
## inverse

makeCacheMatrix <- function(x = matrix()) {
    
    ## let's do our due diligence and make sure we were
    ## at least provided a matrix to work with
    if (class(x)[1] != "matrix") {
        stop("argument to makeCacheMatrix must be a matrix")
    }
    
    ## initialize the inverse matrix to NULL
    ix <- NULL
    
    ## Setter function to set the original matrix
    ## and initialize the inverse matrix to NULL
    set <- function(y) {
      x <<- y
      ix <<- NULL
    }
    
    ## getter function to return the original matrix
    get <- function() x    
    
    ## setter function to set the inverse matrix
    setinverse <- function(invx) ix <<- invx
    
    ## getter function to return the inverse matrix
    getinverse <- function() ix
    
    ## return a list of "function pointers" 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes in a matrix x and returns its inverse
## matrix by either 1) computing the inverse; or 2)
## using a previously cached solution had it been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
        ## get the inverse of x, even if it hasn't been calculated
        ix <- x$getinverse()
        
        ## if the inverse is not null then we have cached data
        ## we can return to the caller which is what we do.
        ## the function call will end here if cached data exists
        if (!is.null(ix)) {
            message("getting cached inverse matrix")
            return(ix)
        }
        
        ## if the inverse was null then let's get the matrix
        ## and store it in the data variable
        data <- x$get()
        
        ## use solve to inverse the matrix stored in data
        ix <- solve(data, ...)
        
        ## now use setinverse to cache the inverse matrix now
        ## that we have computed it
        x$setinverse(ix)
        
        ## finally return the inverse matrix
        return(ix)
}

