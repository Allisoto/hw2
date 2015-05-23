## cachematrix.R
## This function creates a special "matrix" object that can cache its inverse

## 1. create the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

## try test matrix
## x <- matrix(c(1,1,1,3,4,3,3,3,4), 3, 3)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(inverseMatrix) {
        m <<- inverseMatrix
    }
    getinverse <- function() {
        m
    }
    list(set = set, get = get, 
        setinverse = setinverse, 
        getinverse = getinverse)
}


## This function calculates the mean of the special "matrix" 
## created with the above function. 

## 1. Using the output list from "makeCacheMatrix.R", check 
##      to see if the inverse has already been calculated.
## 2. If the inverse has already been calculated, get 
##      the inverse from the cache
## 3. If not, calculate the inverse of the data and set  
##      the value of the inverse matrix in the cache

cacheSolve <- function(outMakeCacheMatrix, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse() 		
    if(!is.null(m)) {				## if the inverse if cached
        message("getting cached data")
        return(m)					## exit program
    }
    data <- x$get()				## otherwise, put the data in 'data'
    m <- solve(data)				## find the inverse of data
    x$setinverse(m)				## call function to cache the inverse
    m							## return the inverse
}
