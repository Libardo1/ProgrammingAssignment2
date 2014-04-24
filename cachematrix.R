##############
# HW 2, Week 3
#Libardo Lopez
# This functions are an example of functional programming on R
#The function makeCacheMatrix define an special object with some added functionality (namely cacheing)

makeCacheMatrix <- function(x = numeric(),set,get,setinverse,getinverse) {
        m <- NULL #initial inverse matrix, NULL
        set <- function(y= matrix()) {#to set a squared matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The function cacheSolve return a matrix that is the inverse of 'x'
## because of the high computational cost of the inverse procedure, first it test for the existence of the inverse matrix
## if it is not empty return the object; otherwaise, calculate the inverse using solve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) { #test if the inverse matrix is not empty
                message("getting Inverse cached data")
                return(m) #shows the inverse matrix
        }
        #if the inverse matrix is empty, then figure the inverse with solve
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

#############################
#Proof

a <- makeCacheMatrix(matrix(5:16,3))

a$get()
#     [,1] [,2] [,3] [,4]
#[1,]    5    8   11   14
#[2,]    6    9   12   15
#[3,]    7   10   13   16

a$getinverse() #first time
#NULL

a$set(matrix(5:8,2)) #set a new matrix
a$get()
#     [,1] [,2]
#[1,]    5    7
#[2,]    6    8  

cacheSolve(a) #first time
#     [,1] [,2]
#[1,]   -4  3.5
#[2,]    3 -2.5

cacheSolve(a) #second time
#getting Inverse cached data
#     [,1] [,2]
#[1,]   -4  3.5
#[2,]    3 -2.5

a$getinverse()
#     [,1] [,2]
#[1,]   -4  3.5
#[2,]    3 -2.5

#test inverse correctness
b = a$getinverse()
a$get() %*% b     #matrix multiplication should show identity matrix
[,1]         [,2]
[1,]    1 3.552714e-15
[2,]    0 1.000000e+00