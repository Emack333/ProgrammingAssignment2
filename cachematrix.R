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
#If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the mean 
#of the data and sets the value of the mean in the cache via the setmean function



#This function gets the inverse of a given matrix, and stores it to be looked up during other same computations  

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

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already 
#been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	inverse_cache <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data.")
		return(inverse_cache)
		}
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inverse_cache)
	inverse_cache
}



#source: findings from internet