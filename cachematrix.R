## The functions creates an special "matrix" object where the inverse of a matrix is computed and stored in cache. In case of no previous
## inverse has been computed, it computes it. Otherwise, cacheSolve will retrieve the value from the cache.

## makeCacheMatrix creates an special "matrix" object. It takes a matrix input "x" and returns four different 
## functions (that set and get the values in the cache), "x" and "m" in the parent environment of the makeCacheMatrix. The inverse matrix of "x" is stored in "m".

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL                                       # m is set to NULL, and it is an object within the makeCacheMatrix environment
        set <- function(y){                             # set will assign the value of y and NULL to x and m in the parent environment
                x <<- y
                m <<- NULL                              # this will clear any value of m that had been cached before
        }
        get <- function() x                             # retrieves the value of x from the parent environment of makeCacheMatrix
        setinverse <- function(solve) m <<- solve       # assigns the value of the inverse to m so it can be accessed
                                                        # after using setinverse()
        getinverse <- function() m                      # defines the getter of the mean m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) # assigns each function to a
                                                                                     # list element, and returns them
                                                                                     # to the parent environment
}


## This function will take as an input the object created by makeCacheMatrix. The function will review if the inverse is already stored in cache. If it is in
## cache, it will get the value and print the message "getting cached data". Otherwise, it will retrieve the matrix from the special "matrix" object and compute
## its inverse then, it will set the inverse in the input object and return the value of the inverse matrix to the parent environment.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()         # gets the inverse computed by the makeCacheMatrix
        if(!is.null(m)){            # the if statement checks if the inverse is already calculated and stored in the cache
                                     # if it is in the cache it gets the value and prints the message, if NULL it has to compute
                                     # the inverse
               message("getting cached data")
               return(m)
        }
        data <- x$get()              # retrieves the special "matrix" object from the object created by makeCacheMatrix
        m <- solve(data, ...)        # computes the inverse of the matrix
        x$setinverse(m)              # sets the inverse in the input object
        m                            # returns the value of the inverse matrix to the parent environment by printing it
}
