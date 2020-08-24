## Put comments here that give an overall description of what your
## functions do

#These functions create a matrix object that caches itself in inverse to
#save computing time. 

## makeCacheMatrix function creates the a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        #Set value of matrix
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        #get value of matrix
        get <- function() x
        #set value of inverse with inverse being the function that returns an inverse matrix
        setinverse <- function(inverse) inv <<- inverse
        #get value of inverse
        getinverse <- function () inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve function looks at the cached inverse of the matrix. If it is null, it will compute the inverse.
#If the inverse is cached, it will return the cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
