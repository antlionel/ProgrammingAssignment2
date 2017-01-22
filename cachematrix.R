## Caching the inverse of a matrix:
## It is important to cache results of time consuming code (such as,
## matrix inversion) so they do not need to be calculated every time.
## The two functions below, makeCacheMatrix and cacheSolve(), are used
## to create an object which stores a matrix and create its inverse.

## To create a special matrix that can cache its inverse, use the
## makeCacheMatrix() function, which is shown below:

makeCacheMatrix <- function(x = matrix()) {
        ## 'x' is a square invertible matrix
        ## Return a list containing functions to:
        ##      1. set the matrix
        ##      2. get the matrix
        ##      3. set the inverse
        ##      4. get the inverse
        ## This is used as the input to cacheSolve()
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set,
             get=get,
             setinv=setinv,
             getinv=getinv)
}

## Use cacheSolve() to calculate the inverse of the matrix returned
## from the makeCacheMatrix() function above. The inverse will be 
## retrieved from the cache directly if the inverse has already been
## calculated and the matrix has not changed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        ## 'x' is the output of makeCacheMatrix()
        ## Return the inverse of the original matrix input to makeCacheMatrix()
        inv = x$getinv()
        
        ## If the inverse has already been calculated:
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        ## Else calculate the inverse
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        ## Sets the value of the inverse in the cache
        x$setinv(inv)
        return(inv)
}