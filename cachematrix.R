## ProgrammingAssignment3_2
Thomas Zachariah
5/28/2017
## This function “makeCacheMatrix” creates a special “matrix” object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The following function “cacheSolve” computes the inverse of the special “matrix” returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}

Testing
TestMatrix <- makeCacheMatrix(matrix(c(5,7,8,20), 2, 2))
TestMatrix$get()
TestMatrix$getInverse()
cacheSolve(TestMatrix)
cacheSolve(TestMatrix)
TestMatrix$getInverse()
TestMatrix$set(matrix(c(-1,5,3,2), 2, 2))
TestMatrix$get()
TestMatrix$getInverse()
cacheSolve(TestMatrix)
cacheSolve(TestMatrix)
TestMatrix$getInverse()

