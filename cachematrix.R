## Nick Ricciarelli
## R Programming
## Programming Assingment 2

## Create matrix obj and cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
          x <<- y
          inverse <<- NULL
        }
        get <- function() {
          x
        }
        setInverse <- function(i) {
          inverse <<- i
        }
        getInverse <- function () {
          inverse
        }
    
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Compute the inverse of the matrix if not already cached, if cached return
cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()

        if(!is.null(inverse)) {
          return(inverse)
        }

        matrix <- x$get()
        inverse <- solve(matrix, ...)
        x$setInverse(inverse)
        inverse
}

