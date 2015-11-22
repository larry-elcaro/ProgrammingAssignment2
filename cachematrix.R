makeCacheMatrix <- function(x = matrix()) {
        ## Establish 4 functions for cahce matris
        ## set() - sets value of matrix
        ## get() - gets contents of matrix
        ## setInv() - invert matrix using solve()
        ## getInv() - get the contents of solve() matrix
        ##
        ## set mtx to NULL
        mtx <- NULL
        ## set function 
        set <- function(y) {
                x <<- y
                mtx <<- NULL
        }
        ## get function
        get <- function() x
        ## setInv() function 
        setInv <- function(solve) mtx <<- solve(x)
        ## getInv() function
        getInv <- function() mtx
        ## list definitions for master function
        list(
                set = set, get = get, setInv = setInv, getInv = getInv
        )
}

cacheSolve <-function(x, ...) {
        ##
        ## Determine cache activity
        ##
        mtx <- x$getInv()
        ## check to see if mtx is empty
        if(!is.null(mtx)) {
                message("Retrieve cached data")
                return(mtx)
        }
        ## 
        ## Get data from matrix
        ##
        data <- x$get()
        mtx <- solve(data, ...)
        x$setInv(mtx)
        mtx
}
