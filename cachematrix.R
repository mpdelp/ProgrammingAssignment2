## Put comments here that give an overall description of what your
## functions do

# Function makeCacheMatrix:
#
# Creating a list containing a function to (1) set and get the value of the matrix and (2)set and get the value of inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y) {
        x<<-y
        inv<<-NULL
    }
    get<-function() x
    setinverse<-function(inverse) inv<<-inverse
    getinverse<-function() inv
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


# Function cacheSolve:
#
# Computing the inverse value of the matrix or returning the cached value if the inverse has already been calculated.

cacheSolve <- function(x, ...) {
    inv<-x$getinv()
    if(!is.null(inv)) {
        message("Data already being cached, getting it now..")
        return(inv)
    }
    data<-x$get()
    inv<-solve(data, ...)
    x$setinv(inv)
    inv
}

# Output sample:
#
# > x = rbind(c(50, 100), c(100,50))
# > m = makeCacheMatrix(x)
# > m$get()
#      [,1] [,2]
# [1,]   50  100
# [2,]  100   50
# > cacheSolve(m)
#              [,1]         [,2]
# [1,] -0.006666667  0.013333333
# [2,]  0.013333333 -0.006666667
# > cacheSolve(m)
# Data already being cached, getting it now..
#              [,1]         [,2]
# [1,] -0.006666667  0.013333333
# [2,]  0.013333333 -0.006666667
