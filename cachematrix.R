## Put comments here that give an overall description of what your
## functions do
## Overall, functions take an invetible matrix
## Determines if the matrix inverse has been created already
## If previously created, it retreives the inverse from Cache
## If not, it calculates the inverse

## Write a short comment describing this function
##This function creates a list of 4 functions:
## set - sets the values of the matrix
## get - gets the values of the matrix
## setinv - sets the values of the inverse matrix
## getiv - gets the values of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL
        set <- function(y) {
                x <<- y
                xinv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) xinv <<- solve
        getinv <- function() xinv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
## This function Checks if the inverse has been calculated
## if so, it is retrieved from cache
## if not, it is created and stored in cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xinv <- x$getinv()
        if(!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        }
        data <- x$get()
        xinv <- solve(data, ...)
        x$setinv(xinv)
        message("inverse calculated")
        xinv
}

