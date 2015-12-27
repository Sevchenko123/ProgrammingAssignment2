makeCacheMatrix <- function(x = matrix()) {    # The makeCacheMatrix function creates a special matrix which is a list that contains a function to
                                               # 1. set the value of matrix, 2. get the value of matrix, 3. set the value of inverse of the matrix, 4. get the value of inverse of the matrix. 
m <- NULL
set <- function(y){
        x <<- y
        m <<- NULL
}
get <- function() x
setinv <- function(solve) m <<- solve
getinv <- function() m
list (set=set,get=get,setinv=setinv,getinv=getinv)
}



cacheSolve <- function(x=matrix(), ...) {       # This function computes the inverse of the special matrix returned by the above function. If the inverse has already been calculates and matrix is unchanged, then the cacheSolve function should retrieve the inverse from the cache
        m <- x$getinv()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }  
        
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setinv(m)
        m
}
