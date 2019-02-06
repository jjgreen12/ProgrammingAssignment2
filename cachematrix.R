## makeCacheMatrix stores both the original matrix and the inverted matrix in the cache

## cacheSolve calculates and displays the result of makeCacheMatrix, either by pulling the cached value
## or by calling the function to invert the function and then displaying the result of that function

## (a) function to set the values of a matrix and create a blank matrix for the future inverse solution
## (b) get the stored original matrix
## (c) set the blank matrix equal to the inverse of the stored original matrix
## (d) function to retrieve the inverse matrix
## Create a list of functions (a-d) to be referenced by a makeCacheMatrix object

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set=set, get=get, 
         setinv=setinv, getinv=getinv)
}


## If the matrix 'inv' is null, it runs the 'solve' function on the input matrix
## If the matrix 'inv' is not null, it displays the cached soulution

cacheSolve <- function(x, ...) {
        inv <- x$getinv() #function from the 'makeCacheMatrix' function
        if(!is.null(inv)) {
            message("getting cahced data")
            return(inv)
        }
        data <- x$get() #function from 'makeCacheMatrix
        inv <- solve(data,...)
        x$setinv(inv) #function from 'makeCacheMatrix'
        inv
}

