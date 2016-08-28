# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# makeCacheMatrix returns a list contains functions to 
        ##     1. set the matrix 
        ##     2. get the matrix 
        ##     3. set the inverse
        ##     4. get the inverse
        ## this list is the input for cacheSolve()

# for this assignment, assume the input matrix is always an invertible square matrix

makeCacheMatrix <- function(x = matrix()) { 
        # initialize the default value of inv to NULL
        inv <- NULL
        # setmatrix makes sure that whenever the input matrix is changed, inv is reset to NULL in the parent environment therefore cacheSolve() will recalculate the inverse 
        setmatrix <- function(y) {
                # assign the input matrix to x in the parent environment 
                x <<- y
                # set the inv to NULL in the parent environment  
                inv <<- NULL
        }
        getmatrix <- function() x
        #calculate matrix inverse using solve() function
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 


cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        # check wether inv has already been calculated, if yes, return the cached inverse 
        if(!is.null(inv)) {
        	     message("getting cached data")
                 return(inv)
        }
        # if inv hasn't been calculated or the matrix is changed, then compute the new inverse
        data <- x$getmatrix()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

# Test run

m <- matrix(data = c(4,6,2,7), nrow = 2, ncol = 2)
myMatrix <- makeCacheMatrix (m)
cacheSolve(myMatrix)
cacheSolve(myMatrix)




