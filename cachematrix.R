## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#Function that creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        set <- function(y) { #Set value of matrix
                x<<- y
                inv <<- NULL
        get <- function() x #get value of matrix
        setInverse <- function(inverse) inv <<- inverse #set value of matrix inverse
        getInverse <- function() inv #Get value of matrix inverse
        list(set = set, get = get, setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

#Function that computes the inverse of the special "matrix" returned by makeCachMatrix
cacheSolve <- function(x, ...) {
               inv <- x$getInverse()
        #Check if inverse has already been calculated
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        return(inv)
}
