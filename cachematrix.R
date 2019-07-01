## Just like the makeVector function in the example, and given the argument of x=matrix()
## I just swap out the m for i to represent 'inverse' and change all the "mean" with "inverse" and it should be exactly what I need

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Similar to the Cache Solve in the second example, I replace all the m's with i's since that's what I did in the CacheMatrix func
## Then I replace all the mean's with inverse's since we're looking for a matrix inverse now
## Lastly I changed line 31 from "m <- mean(data, ...)" to "i <- solve(data, ...)" since we want to solve for the inverse.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
