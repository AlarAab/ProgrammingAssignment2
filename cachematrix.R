## this function creates special "vector" to store (cache) the list of
## of values - set and get the value for vector - and
## set and get the value for inverse matrix

makeCacheMatrix <- function(x = numeric(),...) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setSolve <- function(invertedMatrix) m <<- invertedMatrix
        getSolve <- function() m
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}

## this function calculates the inverse of the matrix
## still first checking to see if the invers has already been calculated
## otherwise it calculates the inverse and sets the value via setSolve()

cacheSolve <- function(x,rows,byrow=TRUE,...) {
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
	  Matrix<-matrix(data,nrow=rows,byrow=byrow)
        m <- solve(Matrix)
        x$setSolve(m)
        m
}
