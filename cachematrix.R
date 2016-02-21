## Put comments here that give an overall description of what your
## functions do

# Function makeCacheMatrix:
#
# Creating a list containing a function to (1) set and get the value of the matrix and (2)set and get the value of inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# Function cacheSolve:
#
# Computing the inverse value of the matrix or returning the cached value if the inverse has already been calculated.

cacheSolve <- function(x, ...) {
    inv<-x$getinv()
    if(!is.null(inv)) {
        message("Data being cached..")
        return(inv)
    }
    data<-x$get()
    inv<-solve(data, ...)
    x$setinv(inv)
    inv
}
