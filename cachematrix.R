## The following functions demonstrate how to cache results
## in objects in R.

## The makeCacheMatrix function will contain the matrix data
## and the cached inverse. It contains getter and setter
## methods for both results, and exposes these with the
## list method.

makeCacheMatrix <- function(mtr = matrix()) {
    i <- NULL
    set <- function(inmtr) {
        mtr <<- inmtr
        i <<- NULL
    }
    get <- function() mtr
    setinverse <- function(ini) i <<- ini
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse,
	getinverse = getinverse)
}


## The cacheSolve function checks whether the inverse of x
## is cached. If not, it calculates the inverse with solve(),
## otherwise, the cached result is returned.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    # Check whether the inverse was cached
    if (!is.null(i)) {
        print("getting cached inverse")
        # return the cached result
        return(i)
    }
    data <- x$get()
    # Calculate the inverse of the matrix in x$get()
    i <- solve(data)
    # Save the result in the cache of the x object
    x$setinverse(i)
    i
}

