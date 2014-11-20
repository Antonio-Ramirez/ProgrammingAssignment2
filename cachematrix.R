## Assignment: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly

## makeCacheMatrix function creates a special "matrix" object, which is a list 
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse        
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function computes the inverse of the special "matrix" returned by
## makeCacheMatrix function.
## first it checks if inverse of the matrix has been calculated. If then, it gets
## the inverse from the cache and skips computation. Otherwise, it calculates 
## the inverse of the matrix and sets the value of the inverse in the cache via
## the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)){
                message("getting cached inverse")
                return(inv)                
        }
        m <- x$get()
        message("computing the inverse of 'x'")
        inv <- solve(m, ...)
        x$setinverse(inv)
        inv
}
