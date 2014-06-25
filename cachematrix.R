## Put comments here that give an overall description of what your
## This function generally returns the inverse of a matrix, but first, it checks
## the function checks the CacheMatrix, to see if the inverse of a matrix already 
## exist, and if it does not exist, it does a fresh calculation. 
## functions do

## Write a short comment describing this function
## This function simply creates a cache for storing matrix of already
## calculated inverses   
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


## Write a short comment describing this function
# The following function calculates the inverse of the 
#special "matrix" created with the above function. However,
# it first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
