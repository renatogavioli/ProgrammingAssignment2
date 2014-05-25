##      This function creates a cache in the form of a list containing four function to: 
##      set and get the value of the matrix, and set and get the value of the inverse of 
##      this matrix.

makeCacheMatrix <- function(x = numeric()) {
        i <- NULL 
        set <- function(y) { ## Sets 'x' as the matrix in this cache, 'i' as NULL
                x <<- y 
                i <<- NULL
        }
        get <- function() x ## Returns the matrix 'x'
        setinverse <- function(inverse) i <<- inverse ## Sets 'i' as the inverse of the matrix (cache)
        getinverse <- function() i ## Returns the inverse matrix 'i'
        list(set = set, get = get, ## Returns a list with the updated values for each function
             setinverse = setinverse,
             getinverse = getinverse)
}


##      This function tests whether the inverse of a given matrix has been calculated and is 
##      present in the cache, and returns the value. If not, it calculates the inverse matrix, 
##      stores it in the cache and returns its value.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()  ## Sets 'i' as the value in the cache
        if(!is.null(i)) {  ## Tests if the value in the cache is NOT NULL
                message("getting cached data") 
                return(i)  ## Returns 'i' if above condition is TRUE
        }
        data <- x$get() ## Else, sets matrix data to 'data'
        i <- solve(data, ...) ## Solves the matrix (calculates the inverse)
        x$setinverse(i)  ## Stores the calculated value to the cache
        i ## Returns the calculated inverted matrix
}
