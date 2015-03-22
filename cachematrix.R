## R PROGRAMMING - COURSE PROJECT - COURSERA (rprog-012)

## User ID          : 5076930
## Submission Login : varun.ramakrishnan@gmail.com

## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## The following two functions are used to cache the inverse of a matrix.

# Below is the "makeCacheMatrix" Function, It creates a list containing a function to
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of inverse of the matrix
#4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

xinv <- NULL  
      
set <- function(y) {
	  x <<- y
	  xinv <<- NULL 
    }

get <- function() x                  # returns the input matrix
setInv <- function(inv) xinv <<- inv # sets the inversed matrix
getInv <- function() xinv            # returns the inversed matrix
      
list(set = set, 
	 get = get,
	 setInv = setInv,
	 getInv = getInv)
		   
}

# Below is the "cacheSolve" function which returns the inverse of the matrix. 
# If the inverse has already been computed, it gets the result and skips the computation. 
# If not, it computes the inverse and sets the value in the cache.
# (Assumption :- The matrix is always invertible.)

cacheSolve <- function(x, ...) {
	  
	inverse <- x$getInv() # Gets the inversed matrix from object x
                          # It will be null if uncalculated.
      
	# If the inversion result is there, then 'gets cached data' and returns the inverse
	  if(!is.null(inverse)) { 
	  message("getting cached data")
	  return(inverse) 
      }
      
    # if not, does x$get to get the matrix object, solves it and returns the solved result.
	data <- x$get()  
    inverse <- solve(data) 
    x$setInv(inverse)      
    inverse                
		
}

## "cachematrix.r" Stub has been forked from https://github.com/rdpeng/ProgrammingAssignment2