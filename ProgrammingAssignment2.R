## Put comments here that give an overall description of what your
## functions do




## MASS contain the fonction ginv that allowed us to make the pseudo inverse of matrix
## it use Moore-Penrose inversion method (usually used to calculate pseudo inverse of non invertibles squares matrices)
## whith that, you are allowed to extand & generalize the inverse notion for non squares matrices 
## as long as they have a value in a algebraic field
## if the matrix is already invertible it work as solve function 

library(MASS)

## inspire from Vector example, change name and function mean to ginv
## take a matrix as paramater and add set & get function re-use matrix value in cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {                 # initialize the set function,use to reset the cache value
                x <<- y
                m <<- NULL
        }
        get <- function() x                  # get take no arg and result the matrix (take in cache)
        setinv <- function(ginv) m <<- ginv  # setinv apply the inverse function
        getinv <- function() m               # permit to use the result of cacheSolve
        list(set = set, get = get,           # define set() get() as name set get
             setinv = setinv,                # setinv() getinv as name setinv getinv
             getinv = getinv)
        
}


## inspire from Vector example, change name setmean/getmean to setinv/getinv and function mean to ginv

cacheSolve <- function(x, ...) {
        m <- x$getinv()                         # m take value of x and getinv function from makeCacheMatrix 
        if(!is.null(m)) {                       # condition to see if you have already something in cache
                message("getting cached data") 
                return(m)
        }
        data <- x$get()                         # data take value of x and get function from makeCacheMatrix 
        m <- ginv(data, ...)                    # m take parameter
        x$setinv(m)                             # from x apply setinv from makeCacheMatrix on m value
        m                                       # return value of m
}

#example of use :

x<-matrix(1:4,nrow = 2, ncol = 2)

xMatrix<-makeCacheMatrix(x)             # makeCacheMtrix(x) in the variable xMatrix
xMatrix$get()                           # get the value of x

a<-c(1,2,3,4)
b<-c(2,4,9,12)
x<-matrix(c(a,b),nrow = 4, ncol = 2)

xMatrix$set(x)                           # reset cache value with an other value of x
xMatrix$get()                            # get the value of x
cacheSolve(xMatrix)                      # apply calcul on the x and cache it
inv <- xMatrix$getinv()                  # return the value in cache without using cacheSolve fucntion

inv

## https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md
## usefull link found in the discussion to understand how works the vector exemple
