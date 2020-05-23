## Functions that cache the inverse of a matrix

## Function makeCacheMatrix creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    ## Inizialize object i
    i<- NULL 

    ## Setter of the Matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## Getter of the Matrix
    get <- function() x
    
    ## Setter of the inverse of the Matrix
    setinverse <- function(inverse) i <<- inverse
    
    ## Getter of the inverse of the Matrix
    getinverse <- function() i
    
    ## Naming the list of elements
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Compute the inverse of the matrix returned by makeCacheMatrix function.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    i <- x$getinverse()
    
    ## There is a valid, cached mean and can return it
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## Getting the matrix from the input object
    data <- x$get()
    
    ## Calculate the inverse using solve()
    i <- solve(data)
    
    ## Setting the inverse in the input object
    x$setinverse(i)
    
    ## Return the inverse matrix
    i
    
}
