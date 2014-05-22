
# The goal is to write a pair of functions that cache the inverse of a matrix.

#Function makeCacheMatrix creates a special "matrix" object that can cache its inverse.
#Function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.



########## makeCacheMatrix ###########
# Function "makeCacheMatrix" creates a special "matrix", which is really a list containing a function to:
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


########## cacheSolve ###########
# The following function computes the inverse of the special "matrix" created with the above function. 
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse 
# of the data and sets the value of the inverse in the cache via the setmean function.


cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}













