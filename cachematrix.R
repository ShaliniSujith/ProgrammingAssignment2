## This program is to cache the inverse of a matrix

## preRequisite: Supplied matrix is a invertible matrix

## makeCacheMatrix - Create a special matrix object that can cache its inverse
##1. Set value of matrix
##2. get value of matrix
##3. Set value of inverse
##4. get value of inverse


makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    
    get <- function()x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function()i
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve function will do the following:
## 1.Inverse the special matrix returned by makeCacheMatrix function 
## 2.If inverse is already been calculated & matrix has not changed
##   then cacheSolve should retrieve inverse from cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)){
        message("getting cache data") 
        return(i)
    }
    data <- x$get()
    
    ## computing inverse using solve function
    i <- solve(data, ...)
    x$setInverse(i)
    i
}