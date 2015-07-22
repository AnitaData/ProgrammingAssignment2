# makeCacheMatrix creates a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of inverse of the matrix
# get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() y
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

# The following function calculates the inverse cached 
# created with the above function. However, it first checks 
# to see if the inverse has already been calculated. If so, it 
# gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix sets in the 
# cache via the setinv function.


cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}