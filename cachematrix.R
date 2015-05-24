## These functions, in tandum, calculate the inverse of a given matrix and store the value of that inverse for future recall.

## begins by setting the inverse to NULL as a placeholder for a future value
## defines a function to set the matrix, x, to a new matrix y, and resets the inverse, i, to NULL
## defines function that returns the matrix, x
## defines function that sets the inverse, i, to a new inverse, inverse
## defines a function that returns the inverse, i
## returns a 'special vector' containing all of the functions defined above


makeCacheMatrix <- function(x = matrix()) {
	i <- NULL  
	set <- function(y) {  
		x <<- y
		i <<- NULL
	}
	get <- function() x  
	setinverse <- function(inverse) i <<- inverse  
	getinverse <- function() i  
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
}


## retrives the cahced value of the inverse, i, of a matrix defined by the function makeCacheMatrix
## if the inverse, i, has already been calculated and cached then returns the cahced value
## if the inverse has not already been cached, then caclulate the inverse and cache this new calculated value for futrure recall

cacheSolve <- function(x, ...) {
	i <- x$getinverse()  
	if(!is.null(i)) {  
		message("getting cached data")
		return(i)
	}
	data <- x$get()  
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
