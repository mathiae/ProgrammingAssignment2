
## makeCacheMatrix makes a list that contains access functions to access/store a matrix and 
## (when already computed) directly access to the inverse. 
## cacheSolve try to find the already computed inverse and if necessary
## compute and store the inverse of the matrix
## The purpose of these two function is to speed up computation in case one has to redo inversion
## several times on the same matrices
##

## Here is makeCacheMatrix that returns the list of access functions

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL # the inverse is not already computed
	set <- function(y) { 
                x <<- y # <<- goes to the parent environment
                i <<- NULL
        }
	get <- function() x 
	setinverse <- function(inverse) i <<- inverse # to store the inverse
	getinverse <- function() i # to get the inverse
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Here is cacheSolve that try to find the already computed inverse and if necessary solve it and store it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse() # already computed ?
	if (!is.null(i)) {
		message("getting cached inverse") # yes notify 
		return(i) # and return it 
	}
	m <- x$get() # get the matrix
	i<- solve(m) # compute the inverse
	x$setinverse(i) # store it for later use
	i # return the inverse
		
}

## run testCacheMatrix() to test the two functions
testCacheMatrix <- function () {
	myCachedMatrix <- makeCacheMatrix() # create one cached matrix 
	mytest <- matrix(1:4,nrow=2,ncol=2) # create a matrix for testing
	myCachedMatrix$set(mytest) # set the cached matrix with the test matrix
	mytestinverse <- cacheSolve(myCachedMatrix) # compute the inverse
	print(mytest%*%mytestinverse) # print multiplication of the matrix by its inverse: ID (2x2) is printed 
	mytestinverse <- cacheSolve(myCachedMatrix) # recompute the inverse (the cache is used)
}