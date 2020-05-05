## This function is used to make the cache matrix.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x ## it fetches the value of x
        setinverse <- function(inverse) i <<- inverse ## It is used to store the inverse.
        getinverse <- function() i ## It is used to fetch the value of the calculated inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function is used to calculate inverse of a matrix.
## If the inverse is already calculated before and is stored in the cache then it fetches it from there.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse() 
        if(!is.null(i)) {## Checks if the inverse is computed before
                message("getting cached data")
                return(i) ## Returns the inverse if present
        }
        data <- x$get()## If inverse not present then it grts the matrix from x
        i <- solve(data, ...) ## It calculotes the Inverse Matrix.
        x$setinverse(i) ## It store the inverse in cache
        i
}
