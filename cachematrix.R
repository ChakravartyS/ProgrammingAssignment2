
## ----------------------------------------------------------------------------

## The functions "makeCacheMatrix" and "cacheSolve" below, together compute the
## inverse of a matrix and cache it
## As long as the original matrix has not changed, there is no need to 
## recompute the inverse matrix, since the cache contains it
##
## Function "makeCacheMatrix" creates a special list containing functions to
##      1. Set the value of the list
##      2. Get the value of the list
##      3. Set the inverse of a matrix
##      4. Get the inverse of a matrix


makeCacheMatrix <- function (x.mtx = matrix()) {
        
        fnctn <- NULL
        
        set.fn <- function (y) {
                x.mtx <<- y
                fnctn <<- NULL
        }
        get.fn <- function () x
        set.inverse.fn <- function (inverse) fnctn <<- inverse
        get.inverse.fn <- function () fnctn
        
        list (set = set.fn
              , get = get.fn
              , setinverse = set.inverse.fn
              , getinverse = get.inverse.fn
              )
}

## ----------------------------------------------------------------------------

## Function "cacheSolve" computes the inverse of the matrix returned by the
## previous function "makeCacheMatrix", assuming that the matrix is invertible
## e.g. matrix (c(1, 0, 5, 2, 1, 6, 3, 4, 0), 3, 3)
##      
## Upon initial invocation the inverse of the matrix is cached (and returned)
##
## When invoked subsequently, the cached inverse matrix is returned, as long as
## the matrix has not changed

cacheSolve <- function (mtx, ...) {
        
        inverse <- mtx $ getinverse ()
        if (!is.null (inverse)) {
                message("Retrieving Cached Data")
                return (inverse)
        }
        
        matrix.data <- mtx $ get ()
        inverse <- solve (matrix.data)
        mtx $ setinverse (inverse)
        inverse
}

## ----------------------------------------------------------------------------