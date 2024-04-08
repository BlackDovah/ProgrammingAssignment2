## This set of functions is meant to store the value of a matrix in an alternative environment.
## Once an object for the caching/storing function has been created with the matrix to be stored, the 2nd function is called
## to check if an inversion of the matrix is also stored in the alternative environment, and if not, one will be created and stored.

## makeCacheMatrix takes an invertible matrix and stores it.
## it can also store the result of inverting the matrix once called via cacheSolve within the same object.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv<- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve takes the object created by makeCacheMatrix as it's argument, and start by checking if its inverted matrix list has a value
## If it does, this means an inversion of this specific matrix exists in the object that stores the matrix with its specific inversion
## If not, that meas that the inversion hasn't yet been done, and the function will compute and store the result within the same object.
## cacheSolve then return the value of the inversion.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
