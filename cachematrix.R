## Put comments here that give an overall description of what your
## functions do
## The following functions create a global object that stores a numeric matrix and caches its inverse

## Write a short comment describing this function
# The makeCacheMatrix creates a special "matrix", which is really a list 
# containing a function to
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse matrix
# 4. Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        if(!is.matrix(x)) {stop("Argument must be a matrix!")} #Verifying argument 
        m <- NULL               # initializing local variable 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve function calculates the inverse matrix of the special "matrix" 
## created with the makeCacheMatrix
## cacheSolve first checks if inverse matrix has already been calculated and 
## if so return it and skip calculation.
## Otherwise, it calculates the inverse matrix of the data and stored it via 
##the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()   # set local variable m with the value of global m 
        if (!is.null(m)) {    # if m not null then message and return value
                message("Getting cached data")
                return(m)
        }
        data <- x$get()   # Set data variable as the matrix passed as an argument 
        m <- solve(data, ...) # get the inverse of the matrix and assign to local variable m 
        x$setinverse(m)       # set the global m with the value of local m
        m                     # Display local m with the inverse matrix of the argument matrix
}


# Unit testing 

# onematrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
# onematrix$get()
# onematrix$getinverse()
# cacheSolve(onematrix)
# cacheSolve(onematrix)

# onematrix = makeCacheMatrix(matrix(c(1,2,4,7,6,8,5,3,9), nrow=3, ncol=3))
# onematrix$get()
# onematrix$getinverse()
# cacheSolve(onematrix)
# cacheSolve(onematrix)

# onematrix = makeCacheMatrix(matrix(c(1,1,1,1,1,2,3,4,1,3,6,10,1,4,10,20), nrow=4, ncol=4))
# onematrix$get()
# onematrix$getinverse()
# cacheSolve(onematrix)
# cacheSolve(onematrix)

# onematrix = makeCacheMatrix(matrix(c(-1, -2, 1, 1), nrow=2, ncol=2))
# onematrix$get()
# onematrix$getinverse()
# cacheSolve(onematrix)
# cacheSolve(onematrix)
