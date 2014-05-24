## Matrix inversion is usually a costly computation and there may be some
##benefit to caching the inverse of a matrix rather than computing it
##repeatedly

## This function, creates a special "matrix", which is
## really a list containing a function to

##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the value of the matrix
##4.  get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has been already been calculated. If so, it `get`s the inverse 
## from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value 
## of the inverse in the cache via the `setmatrix` function.

cacheSolve <- function(x=matrix(), ...) {
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}