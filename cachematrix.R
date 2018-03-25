## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## 1 - Setting the value for Cache Matrix
## 2 - Getting the value for Cache matrix
## 3 - Setting the value of the Solve to find the inverse Matrix
## 4 - Getting the value of the Solve

makeCacheMatrix <- function(x = matrix()) {
        mc  <-  NULL  ## mc = make cache matrix
        set <- function(y){
                x <<- y
                mc <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) mc <<- solve
        getsolve <- function() mc
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mc <- x$getsolve()
        if(!is.null(mc)){
                 message("Getting cached data!")
        return(mc)
        }
        data <- x$get()
        cm <- solve(data, ...)
        x$setsolve(mc)
        mc
}
