## The following two functions cache and compute the 
## inverse of a matrix.

makeCacheMatrix <- function(mtx = matrix()) {
        inverse <- NULL
        set <- function(x) {
                mtx <<- x;
                inverse <<- NULL;
        }
        get <- function() return(mtx);
        setinv <- function(inv) inverse <<- inv;
        getinv <- function() return(inverse);
        return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## This function computes the inverse of  `makeCacheMatrix` above. 
## If the inverse has already been calculated, then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(mtx, ...) {
        inverse <- mtx$getinv()
        if(!is.null(inverse)) {
                message("From cache:")
                return(inverse)
        }
        data <- mtx$get()
        inverse <- solve(data, ...)
        mtx$setinv(inverse)
        return(inverse)
}