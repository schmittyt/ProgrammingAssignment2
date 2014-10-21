##This script has 2 main functions to calculate the inverse of a matrix
# If the matrix has already been calculated, the code will pull the results
#   from a cache rather than recalculate it.
##

## This function will return a list containing a function to
# 1 set the value of the vector
# 2 get the value of the vector
# 3 set the value of the inverse
# 4 get the value of the inverse
##

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL                       #set the local variable m where the inverse matrix will be saved
        set <- function(y) {            #resets the global m and assigns the matrix to global x
                x <<- y   
                i <<- NULL
        }
        get <- function() x             #returns the x it finds, (local then global)
        setinverse <- function(inverse) i <<- inverse   # sets the global inverse
        getinverse <- function() i      #returns local/global inverse it finds 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)   #returns the guts of all 4 functions to caller
}


## This function calculates the inverse of the supplied matrix.  
##      If one has previously been calculated for the same matrix, the cached
##      version is returned rather than recalculating the inverse again.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()     #Pulls whatever it finds in the i variable
        if(!is.null(i)) {       #If the inverse is stored in i (i isn't null)...
                message("getting cached data")  #Print a message indicating we aren't recalculating
                return(i)       #return the non-null value found & ends function
        }
        data <- x$get()         #since we didn't exit, we need to grab the matrix from memory
        i <- solve(data, ...)   #calculate the inverse
        x$setinverse(i)         #set the inverse into memory
        i                       #return the calculated inverse
}
