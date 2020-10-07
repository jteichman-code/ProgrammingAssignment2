## Assignment:  Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(mat = matrix()) {
    ## Initializes the inverse
    inv <- NULL
    
    ## Sets the matriX
    set_matrix <- function(matrix) {
        mat <<- matrix
        inv <<- NULL
    }
    ## Returns the matrix
    get_matrix <- function() mat
    
    ## Sets the inverse of the matrix
    set_inverse <- function(inverse) inv <<- inverse

    ## Gets the inverse of the matrix
    get_inverse <- function() inv
    
    ## Returns a list of the methods (items above)
    list(set_matrix = set_matrix,
        get_matrix = get_matrix,
        set_inverse = set_inverse,
        get_inverse = get_inverse
    )
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix() above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve() should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inverse()
    if (!is.null(inv)) {
        message("getting cached data")
      return(inv)
    }
  
    mat <- x$get_mat()
    inv <- solve(mat, ...)
    x$set_inverse(inv)
    inv
}