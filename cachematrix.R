## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    inv_matrix <- NULL
    
    #input the inverse data directly to memory
    set <- function(y = matrix()) {
        x <<- y
        inv_matrix <<- NULL
    }
    
    #calling the matrix from memory/cache
    get <- function() x
    
    #input directly the inverse matrix output
    set_inverse <- function(defined_matrix) inv_matrix <<- defined_matrix
    
    #getting the inverse result from memory within makeCacheMatrix list of function
    get_inverse <- function() inv_matrix
    
    #making a list to call from makeCachematrix
    list(set = set, 
         get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## Write a short comment describing this function
## Based on makeCachematrix above. The data has been store to memory
## in the case to x in parent environment

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## call the mean value from memory, if NULL then continue calculate
    inv_matrix <- x$get_inverse()
    if(!is.null(inv_matrix)) {
        message("getting cached data")
        return(inv_matrix)
    }
    
    ## call the data from memory and caculate the inverse
    data <- x$get()
    inv_matrix <- solve(data, ...)
    
    #continue to store inverse calculation to memory
    x$set_inverse(inv_matrix)
    inv_matrix
}
