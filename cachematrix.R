# makeMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix
makeMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) m <<- inverse
        getinv <- function() m
        list(set=set, get=get, 
             setinv=setinv, 
             getinv=getinv)
}


# This function returns the inverse of the matrix (do not work with an uninvertible matrix.)
# it checks the cache first to see if the matrix has already been inverted  to avoid computing the values
# if it's not the case, it computes it and cache it
cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data.")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}

## Sample run:
## > x <- rbind(c(3,4),c(5,2))
## > m <- makeMatrix(x)
## > m$get()
##      [,1] [,2]
## [1,]    3    4
## [2,]    5    2

## m$getinv()
## NULL

## First run, so no cached data
## > cacheSolve(m)
##            [,1]       [,2]
## [1,] -0.1428571  0.2857143
## [2,]  0.3571429 -0.2142857

## Try a second time, this should come from the cache now.
## > cacheSolve(m)
## getting cached data.
##            [,1]       [,2]
## [1,] -0.1428571  0.2857143
## [2,]  0.3571429 -0.2142857
## > 

## m$getinv()
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
