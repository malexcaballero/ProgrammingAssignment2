## Base on the examaple caching the mean of a vector adapt the main idea to the problem 
## caching the inverse of a matrix

## there are 4 functions, first one is set() use to set the value of the matrix.
## second is get() to recover or show the original matrix x.
## third is setsolve() this is use to assing the inverse of the matrix to the vaiable m
## in the parent environment. 
## four getsolve() show the invers of a matrix once that is already calculated with the 
## cachematrix function and is save in the object m.

## the first part of the function if for create the special object that returns 4 functions
## and 2 variables that save the environment in which they were created.
## you can used for the example the vector v1 <- matrix(c(1, 3, 1, 2,2,0,3,1,1), nrow = 3, ncol = 3)
## mymatrix <- makeCacheMatrix(v1)


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


## this is the complement for the function makecachematrix, this funtion return the inverse 
## of a matrix if it in the cache memory that mean that is already save in the data object m
## if it not in the cache memory the function calculate the inverse and save it in m
## cacheSolve(mymatrix)  
## answer: [,1]  [,2] [,3]
##     [1,] -0.25  0.25  0.5
##     [2,]  0.25  0.25 -1.0
##     [3,]  0.25 -0.25  0.5


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
