## These functions allow calculating the inverse matrix,
## and avoid repeating the calculate procedure setting the inverse matrix into a vector. 


## makeCacheMatrix create a list of functions to set or get the value of the matrix, and set or get
## the value of its inverse matrix
## Previously, the function check if the matrix is invertible matrix (square and regular matrix)

makeCacheMatrix <- function(x = matrix()) {
        x <- as.matrix(x)
        
        if (nrow(x) != ncol(x)) {
                print("This matrix isn't square matrix. It can't to solve the inverse matrix.")
                return()
        }
        if (det(x) == 0) {
                print("This matrix isn't invertible matrix. Its determinant is 0")
                return()
        }
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinver <- function(inver) inv <<- inver
        getinver <- function() inv
        list(set = set, get = get,
             setinver = setinver,
             getinver = getinver)
}


## cacheSolve determines if the inverse matrix has already calculated and returns it.
## If the inverse matrix doesn't exist, cacheSolve  calculates it and sets into de vector for future use
## and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinver()
        if(!is.null(inv)) {
                print ("getting cached data")
                return(inv)
        }
        mdata <- x$get()
        inv <- solve(mdata, ...)
        x$setinver(inv)
        inv
}
