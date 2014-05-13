## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creats a special "matrix" that has an inverse.
## It is a list of following functions
## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set   <- function(y){
        x     <<- y
        inver <<- NULL
        return(x)
    }
    get   <- function() x
    setinver <- function(inverse) inver <<- inverse
    getinver <- function() inver
    list(set = set, get = get, setinver = setinver, getinver = getinver)
}


## cacheSolve calculates the inverse of the special "matrix"
## If the inverse has been calculated, it retrive the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getinver()
        if (!is.null(inver)) {
            message("getting cache data")
            return(inver)
        }
        data  <- x$get()
        inver <- solve(data)
        x$setinver(inver)
        inver
}
