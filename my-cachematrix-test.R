## simple method to test the CacheMatrix methods

source('~/GitHub/ProgrammingAssignment2/cachematrix.R')

testCacheMatrix <- function() {
    ## create a four by four matrix with random normal values
    message("\nCreating test matrix.")
    m <- matrix(rnorm(16), 4, 4)
    print(m)
    
    ## for testing purposes, solve the matrix and save the inverse
    message("\nSolving the test matrix for testing purposes.")
    im <- solve(m)
    print(im)
    
    ## now let's make the CacheMatrix "object" (list)
    message("\nCreating the CacheMatrix from the makeCacheMatrix(m) call.")
    cm <- makeCacheMatrix(m)
    print(summary(cm))
    
    ## call cacheSolve comparing the returned inverse to the one
    ## we pre-calculated; we should not get a message about using
    ## a cached inverse matrix yet; the result should be all TRUE
    message(paste("\nCalling cacheSolve(...) and comparing results the ",
                  "first time.\nComparison should be TRUE.", sep=""))
    o <- im == cacheSolve(cm)
    print(o)
    
    ## repeat, but this time we should see the message that the
    ## cached matrix is being returne; the result should be all TRUE
    message(paste("\nCalling cacheSolve(...) and comparing results ",
                  "the second time.\nComparison should be TRUE.\n",
                  "Expecting the cached matrix.", sep=""))
    o <- im == cacheSolve(cm)
    print(o)
    
    ## let's check it against a "bad" matrix
    ## really just a new matrix that won't match
    xm <- matrix(rnorm(16), 4, 4)
    
    ## this time we compare to cacheSolve we should still see the
    ## cached inverse matrix returned, but all the values from the
    ## logical compare should be FALSE unless we are very, very lucky
    message(paste("\nCalling cacheSolve(...) and comparing results ",
                  "against a new matrix.\nComparison should be FALSE ",
                  "unless we are supremely lucky!\nExpecting the ",
                  "cached matrix.", sep=""))
    o <- xm == cacheSolve(cm)
    print(o)
}