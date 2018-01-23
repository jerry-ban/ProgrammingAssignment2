## Put comments here that give an overall description of what your
## functions do

## store a matrix, also store a matrix's inverse

#to test it, run following script:
# A <- matrix( c(5, 1, 0, 3,-1, 2,  4, 0,-1), nrow=3, byrow=TRUE)
# y<-makeCacheMatrix(A)
# cacheSolve(y)
# cacheSolve(y)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inv <<- inv
    getinverse <- function() inv
    list( set = set, get = get,  setinverse = setinverse, getinverse = getinverse)
}


## return the inverse of a matrix, if inversible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached matrix inverse")
        return(inv)
    }
    data <- x$get()
    dtrmnt = det(data) 
    if(dtrmnt==0){
        warning("matrix is not inversible!")
        return
    }
    else{
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
    }
}


