## Below are two functions that are used to create a special object that stores a matrix 
## and cache the inverse of this matrix.

## Function makeCacheMatrix  creates a special "matrix" object that can cache its reverse.

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y){
		x<<-y
		m<<-NULL
	}
	get<-function() x
	setinverse<-function(inverse) m<<-inverse
	getinverse<-function() m
	list(set=set,get=get,
	setinverse=setinverse,
	getinverse=getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated, 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinverse()
        if(!is.null(m)){
        	message("getting cached data")
        	return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
}
