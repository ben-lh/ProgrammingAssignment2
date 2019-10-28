## Put comments here that give an overall description of what your
## functions do




## MASS contain the fonction ginv that allowed us to make the pseudo inverse of matrix
## it use Moore-Penrose inversion method (usually used to calculate pseudo inverse of non invertibles squares matrices)
## whith that, you are allowed to extand & generalize the inverse notion for non squares matrices 
## as long as they have a value in a algebraic field
## if the matrix is already invertible it work as solve function 
library(MASS)

## inspire from Vector example, change name and function mean to ginv
## take a matrix as paramater and add set & get function re-use in cacheSolve
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(ginv) m <<- ginv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## inspire from Vector example, change name setmean/getmean to setinv/getinv and function mean to ginv

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- ginv(data, ...)
        x$setinv(m)
        m
}

#example of use :
x<-matrix(1:4,nrow = 2, ncol = 2)

xMatrix<-makeCacheMatrix(x) #value of makeCacheMtrix in xMatrix
xMatrix$get() #get the value of x

a<-c(1,2,3,4)
b<-c(2,4,9,12)
x<-matrix(c(a,b),nrow = 4, ncol = 2)

xMatrix$set(x) #reset cache value with an other value of x
xMatrix$get() #get the value of x
cacheSolve(xMatrix) #apply calcul on the x and cache it
inv <- xMatrix$getinv() #return the value in cache without using cacheSolve fucntion

inv