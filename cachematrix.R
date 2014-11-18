## These two functions use a modified version of the makeVector and cachemean functions provided in class.
## There are 2 main differences:
## 1.The input to the "make" function is a square matrix instead of a vector.
## 2.Both functions use the function solve instead of mean to calculate the matrix inverse
## All function names and variables have been updated to reflect the code intent

## This function will create a list of 4 functions to set and get the values for the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function either retrieves the chached value of the inverse of matrix x. 
## If the value is not cached it will calculate it using the solve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
