## makeCacheMatrix : This function creates a special "matrix" object 
## that can cache its inverse. It inputs a matrix as its argument and creates
## a list of the following four nested functions which are subsettable: 
## 1. set function: To Acquire a matrix and make it globally available across 
##    functions.  
## 2. get function: To return the matrix 
## 3. setinverse function: To save the inverse of the matrix in cache.
## 4. getinverse function: To return the inverse of the matrix which is stored
##    in cache



makeCacheMatrix <- function(x = matrix()) {
        
        # Assign NULL value to variable m. 
        m <- NULL
        # set function
        set <- function(y) {
                # Global Assignment of the value of variable y to variable x.
                # So that value of x is accessible outside this function. 
                # y should be a matrix 
                x <<- y
                # Global Assignment of NULL value to variable m
                # Initialize the matrix
                m <<- NULL
        }
        
        # get function 
        # Returns the assigned matrix which is the value of variable x
        get <- function() x
        
        # setinverse function
        # Global Assignment of the value of variable inverse to variable m
        setinverse <- function(inverse) m <<- inverse
        
        # getinverse function
        # Returns the value of variable m
        getinverse <- function() m
        
        # Create a list containing all the above defined functions so that
        # they are subsettable
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve :This function calculates the inverse of the matrix which was 
## returned by makeCacheMatrix function. Before it calculates the value it
## checks if the inverse of the matrix has been calculated before and if so, 
## it retrieves the value from the cache. This avoids the need to recalculate
## the inverse of the matrix again and thereby saves system resources. 


cacheSolve <- function(x, ...) {
        # Assign the value returned after calling the function getinverse
        # which is part of the function makeCacheMatrix to the variable m
        # The makeCacheMatrix function should be assigned to the argument x
        m <- x$getinverse()
        
        # Check if the value of m is not NULL. If so, then print the message,
        # return the value of m from the cache and exit the function.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # If the value of m is null, then call the get function which is part
        # of the makeCacheMatrix function to return the matrix and assign it
        # to the variable data
        data <- x$get()
        
        # Calculate the inverse of the matrix using the solve() function and 
        # assign it to the variable m
        m <- solve(data, ...)
        
        # Call the function setinverse which is part of the makeCacheMatrix 
        # function and save the inverse of the matrix in cache
        x$setinverse(m)
        
        # Print the inverse of the matrix
        m
}
