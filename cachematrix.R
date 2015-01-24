## Shell code taken from https://github.com/rdpeng/ProgrammingAssignment2 read me file.
###vector mean functions modified for a square matrix that is assumed invertible to be
###inverted and cached. 

## Put comments here that give an overall description of what your
## functions do

## This function is used to create matrix function to create a cached version of the inverse of the input matrix.

makeCacheMatrix <- function(x = matrix()){
        ## Set the varible solve as a null.
        solve <- NULL 
        ## Create the function "set" which will set the input varible y to x and set the solve varible to null.
        ## This is done outside of the environment of the function "set".
        set <- function(y){
                x <<- y
                solve <<- NULL} 
        ## Create the function "get" which will return the varible x.
        get <- function() {x}
        ## Create the function "setsolve" which assigns the input varible to the solve varible.
        ## This is done outside of the environment of the function "setsolve".
        setsolve <- function(test) {solve <<- test}
        ## Create the function "getsolve" which will return the varible solve.
        getsolve <- function() {solve}
        ## Returns a vector with the defined functions assigned to names.
        ## set=set --> creates an object named "set" and defines that object as the "set" function
        list(set = set, get = get, setsolve = setsolve,getsolve = getsolve)
}

## This function will take in a varible. test to see if the inverse of the input is saved in the cachedmatrix and if not inverse the input and cache it for latter use.
cacheSolve <- function(x, ...) {
        ## Create a varible called test and assign it the varible solve via the getsolve function called by using $getsolve().
        test <- x$getsolve()
        ## Check to see uf the test matrix is not null.  
        if(!is.null(test)){
                ## If there is a value other than Null for test display the following message:
                message("getting cached data")
                ## Return the varible test and end the code execution.
                return(test)
        }
        ## If test is Null the following code will execute.
        ## assign varible retrieved x viat the function call $xget
        retrieved <- x$get()
        ## Assign test to be the inverse of retrieved with the arguments extended from the function in which the solve function was called.
        test <- solve(retrieved, ...)
        ## Set the cached matrix solve as the updated varible test via the $setsolve function call. 
        x$setsolve(test)
        ## Return test
        test
}
