## The aim of these functions is to limit the numbers of inversion matrix calculations.
## Once an inversion of a particular matrix is calculated, it's cached for further usage.

## The ouput of makeCacheMatrix function is the list of 4 functions. 
## They allow to add and retreive the specific matrix and its inversion. 
## By default, the inversion is set up as NULL.

makeCacheMatrix <- function(matrix = matrix()) {
    inversion <- NULL
    setmatrix <- function(x) {
        matrix <<- x
        inversion <<- matrix()
    }
    getmatrix <- function() matrix
    setinversion <- function(x) inversion <<- x
    getinversion <- function() inversion
    list(setmatrix = setmatrix,
         getmatrix = getmatrix,
         setinversion = setinversion,
         getinversion = getinversion)
}

## The aim of cacheSolve function is to calculate an inversion for a given matrix.
## At the beginning, the function checks if the inversion was already calculated, i.e. inversion is not a null matrix.
## If so, the inversion saved in cache is used. Otherwise, the inversion is calculated with solve() function.

cacheSolve <- function(matrix, ...) {
    inversion <- matrix$getinversion()
    if(!is.null(inversion)) {
        message("inversion cached")
        return(inversion)
    }
    x <- matrix$getmatrix()
    inversion <- solve(x)
    matrix$setinversion(inversion)
    inversion
}
