## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #set value
        m <- NULL
        set<- function(y){
                x<<- y
                m <<- NULL
        }
        #get value
        get <- function() {x}
        #set and get values for inv
        set_inv <- function(inverse) {m<<- inverse} 
        get_inv <- function() {m}
        
        list(set = set, get= get, set_inv = set_inv, get_inv = get_inv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        #conditions from previous function (makecachematrix)
        m <- x$get_inv()
        
        if(!is.null(m)){
                #output: message and value
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$set_inv(m)
        m
        
}

