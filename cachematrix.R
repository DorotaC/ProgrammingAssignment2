## To complete this assignement I used the code provided in the description and read "Demystifying makeVector()"
## and "Programming Assignment 2: makeCacheMatrix() as an Object".

## makeCacheMatrix produces a list of 4 elements - functions. Set functions contain pointers
## to the parent environment (<<), which enable modifications of the values (x, s) when function is called.
## Set function can be used when we want to compute inversion of another matrix without initializing the whole object 
## of type makeCacheMatrix"; it does the same as the two first lines of the "makeCacheMatrix" function. It "clears"
## the memory assigned to variable s in case the matrix that is to be inverted is changed.
## setsolve function assign value of the inverted matrix to the s, which is defined in the parent enviroment
## both get functions (get and getsolve) are used to find the correct values in the parent environment

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## cacheSolve returns an inverted matrix. First it assignes a value of a matrix, previously processed through makeCacheMatrix 
## function, then checks if the inversion has already been done and the inverted matrix is ready. If so, it 
## returns previously computed inverted matrix. Otherwise, it inverses matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
