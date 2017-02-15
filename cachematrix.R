## The functions below will display the inverse of a given square matrix in a memory efficient way.
## It first checks the cache to see if the inverse is already available and displays the result if it is.
## In case it is not, matrix inverse is calculated and displayed. The same will be stored in the memory for future 
# calls.

## The function clears the cache of the previosuly stored inverse and stores the new inverse value of the new matrix 
## and stores the functions in a list, which can be called later in another function.


makeCacheMatrix <- function(x = matrix()) {
# Initializing 'Inv'. If not, get error everytime makeCacheMatrix is called    
    Inv<- NULL
    
# A new matrix can be passed by "set" function without calling makeCachematrix. In that case, 'x' needs 
# to be updated to the new matrix and 'Inv' needs to be reset. This "set" function has no significance in this exercise, but come in handy 
# in other situation
    set<-function(y){
        x<<-y
        Inv<<-NULL
    }
    
# retrieve the currently stored matrix in 'x'    
    get <- function() x
    
# storing the value in 'inverse' into 'x'. 'inverse' gets its value from computation done in some other function    
    setinv <- function(inverse) Inv <<- inverse

    # retrieving the value in 'inv'    
    getinv <- function() Inv

# finally, the results of above 4 functions are stored in a list and are named. This enables the second function 
# to call the function inside makeCacheMatrix by '$' operator    
   list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function is intended to return the inverse of a square matrix from the cache, if available. If not available
## it calculates and returns the value
cacheSolve <- function(x, ...) {

# retrieving value in 'Inv' for the next step    
    Inv <- x$getinv()
    
# If inverse is avilable in'Inv', it returns the inverse and function ends   
    if(!is.null(Inv)) {
        message("getting cached data")
        return(Inv)
    }
    
# executed only if 'Inv' is NULL.
    
# 'x' is the new matrix and is passed used to return the value in "get" function in makeCacheMatrix 
#and stored in 'data'
    data <- x$get()
    
# "Solve(data)" calculates the inverse of the matrix stored in 'data' and stores in 'Inv' in this environment   
    Inv <- solve(data)
    
# Stores the matrix in 'Inv' and make it available in parent environment    
    x$setinv(Inv)

# Displays inverse matrix in 'Inv'    
    Inv
}
