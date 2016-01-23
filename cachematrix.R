## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


## function to save matrix data in the cache, set() for the matrix, setmatrix() for the inverse value.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    #set the matrice we are going to use 
    #ensure that the object m which will store the inverse of y does not contain any value  
    set <- function(y) {
        print(y)
        x <<- y
        j <<- y 
        m <<- NULL
    }
    #to get the value 
    get <- function() x
    #to set the matrice inverse 
    setmatrix <- function(inverse) m <<- inverse
    #to get the matrice inverse
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## Gives the inverse matrix or calculates it if needed
    # If the cache for the inverse matrice is empty : calculates the inverse matrix with makecachematrix solve(get())
    # If the cache for the inverse matrice is full, check if the cache is similar to the makecachematrix solve(get())
    # If it wasn't, it calculates the inverse matrix with makecachematrix get() / If it was, gives the cache value

cacheSolve <- function(x= matrix(), ...) {

    m <- x$getmatrix()
    if(!is.null(m))  {
        if(is.matrix(x$get()) && is.matrix(solve(m)) && dim(x$get()) == dim(solve(m)) && all(x$get() == solve(m))) {
           message("getting cached data")
            return(m) 
        }
    message("the cache inverse matrix and the matrix don't match. The last matrix will have it inverse calculated")
        
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
    
}




