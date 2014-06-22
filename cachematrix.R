## Calculating the inverse of a matrix can be time consuming. This set of functions allows the
## result of the inverse to be cached once calculated, and to access that cached value when
## it is available when performing future calculations of the matrix inverse.
## makeCacheMatrix allows the matrix to be created, and then cacheSolve will automatically
## use the cached result if available, or calculate the ivnerse if it is not.
## These functions assume that the matrix is invertible.

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the (cached) value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) s <<- solve
    getinv <- function() s
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The following function calculates the inverse of the special "matrix" created with the 
## makeCacheMatrix function. However, it first checks to see if the inverse has already been 
## calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, 
## it calculates the inverse of the matrix and sets the value of the inverse in the cache via 
## the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getinv()    
    if(!is.null(s)) {
        message("getting cached data")        
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinv(s)
    s
}


## This just runs a short battery of tests on the functions. It is not exhaustive and should
## not be considered a complete set of unit tests. This function is not required.

runTest <- function() {
    print("Testing")
    vals <- matrix(c(1, 2, 0, 3, 1, 6, 6, 1, 2), 3, 3)
    mymatrix <- makeCacheMatrix( vals )    
    print("Original cache (NULL expected)")
    print(mymatrix$getinv())
    print("cacheSolve (should create cache)")
    print(cacheSolve( mymatrix ))
    print("Current cache (expected same as above)")
    print(mymatrix$getinv())
    print("cacheSolve (expected same as above, from cache)")
    print(cacheSolve( mymatrix ))
    print("Check inverse (should be identity matrix)")
    print( vals %*% cacheSolve( mymatrix ) )    
}