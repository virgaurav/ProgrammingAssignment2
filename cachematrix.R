## The two functions 'makeCacheMatrix' and 'cacheSolve' can be used to
## create a special matrix object and cache the inverse of a matrix. 


## The function 'makeCacheMatrix' creates a special "matrix", which is 
##really a list containing a function to

##set the value of the vector
##get the value of the vector
##set the value of the mean
##get the value of the mean

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


## The function 'cacheSolve' calculates the inverse of the special
## "matrix". First it checks if the inverse has already been calculated
## and if yes, then it gets the value of inverse from the cache. If the
## had not been calculated before then it calculates the inverse and
## stores it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getmatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    else{
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        return(m)
    }
    
}
