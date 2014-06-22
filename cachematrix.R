## Matrix inversion can be time consuming task. 
## Functions included below support inversion of matrix and caching result 
## which can be very quickly restored when next time inversion of matrix 
## (which was earlier processed) is needed. 
##
## How it works:
##
## Lets "c" is constructed matrix
## v<-makeCacheMatrix(c) will create object which includes matrix given as argument 
##                       and 'place' for inverted cached matrix
## v$getInv() will return NULL
## z<-cacheSolve(v) will inverse matrix, return it and additonally save it into v 
## v$getInv() this time will return already cached matrix



## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	invertedM <- NULL
	
	set <- function(y) {
                x <<- y
                invertedM <<- NULL
        }
	 get <- function() x
	
	setInv <- function(inv) invertedM <<- inv
	
	getInv <- function() invertedM 

	list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## - If the inverse has already been calculated , then the cachesolve should 
##   retrieve the inverse from the cache
## - If the inverse hasn't been calculated, inversion is performed, result is saved in "matrix"
##   object created by makeCacheMatrix function and returned. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	 invertedM <- x$getInv()
	
	#If the inverse has already been calculated
	 if(!is.null(invertedM )) {
            message("getting cached matrix")
            return(invertedM )
        }
	#Inverse hasn't been calculated, lets do it now
        data <- x$get()
        invertedM <- solve(data)
        x$setInv(invertedM )
        invertedM 

}