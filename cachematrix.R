## makeCacheMatrix will take a matrix and provide get/set functions to store
## the matrix and it's inverse value (which is calculated seperately)

# Usage example: 
# m <- matrix(data = c(1,12,11,23,4,5,6,9,2,23,32,14,54,65,7,89), nrow=4, ncol=4)
# mtrx <- makeCacheMatrix(m)
# cacheSolve(mtrx)
# Did not find cached matrix, computing and caching the inverse.
# [,1]         [,2]        [,3]         [,4]
# [1,] -0.097781181  0.009056789 -0.02429640  0.054624351
# [2,]  0.223526854 -0.286584040  0.16547075  0.060665362
# [3,] -0.009198597  0.046390047  0.01125008 -0.029184038
# [4,]  0.004112425  0.019342579 -0.01222383 -0.004424402
# > cacheSolve(mtrx)
# Using cached matrix
# [,1]         [,2]        [,3]         [,4]
# [1,] -0.097781181  0.009056789 -0.02429640  0.054624351
# [2,]  0.223526854 -0.286584040  0.16547075  0.060665362
# [3,] -0.009198597  0.046390047  0.01125008 -0.029184038
# [4,]  0.004112425  0.019342579 -0.01222383 -0.004424402

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) i <<- inv
    getInverse <- function() i
    
    list(set = set, get= get, setInverse = setInverse, getInverse = getInverse)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    i <- x$getInverse()
    if(!is.null(i)) {
        message("Using cached matrix")
        return(i)
    }
    message("Did not find cached matrix, computing and caching the inverse.")
    data <- x$get()
    i <- solve(data)
    x$setInverse(i)
    i
    
}
