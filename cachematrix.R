####################################
#### R Programming Assignment 2 ####
## Peer Assignment ### April 2014 ##
####################################

## The following code consists of two fuctions to calculate the
## inverse of a matrix and cache the result.

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x=matrix()) { # Take a matrix of n x n.
        m <- NULL # The inverse m is assigned as NULL at this point.
        set <- function(y) { 
                x <<- y
                m <<- NULL
        }
	# We proceed to create a list that will contain information of the matrix
	# so that we may use it to cache the inverse matrix.
        get <- function() x
        setinv <- function(invr) m <<- invr
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { # This function will recieve the result of makeCacheMatrix
        m <- x$getinv() # m is where we store the inverse matrix
        if(!is.null(m)) { # If m is to be recognized in our cache, a message will return along with m.
                message("...huh, I've seen that one before...")
                return(m)
        }
        data <- x$get() # In case it is a new matrix, the inverse will be calculated.
        m <- solve(data, ...)
        x$setinv(m)
        return(m) # Returns the inverse matrix.
}

################## EXAMPLE ####################
## To test it, write out something similar to the following:
w <- c(1,3,2,4) # The elements of our matrix.

## makeCacheMatrix takes w and stores the elements in a matrix of 2 by 2.
a <- makeCacheMatrix(matrix(w,2)) 

## Returns the inverse of the matrix given.
cacheSolve(a)

## Finally, if you run this last line of code, you'll get a message that 
## it's an answer that's already in Cache.

cacheSolve(a)

## Running the example code gives us:
# w <- c(1,3,2,4) # The elements of our matrix.
# a <- makeCacheMatrix(matrix(w,2)) 
# cacheSolve(a)
#     [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
# cacheSolve(a)
# ...huh, I've seen that one before...
#     [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
 