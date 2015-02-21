## Hi, fellow! Hope, you enjoyed this task as I did) Good luck!
## So, the following functions are needed to economize the time and resources
## of our computers. As R feature is to store all used data in RAM, it can take
## quite some time until your code is done. 
## In order to optimize the process we introduce caching. Basicaly, we check
## whether we are doing the work we'd done before. If the answer is yes, we just 
## take out the result from the memory.

## The first funtion returns a list with 4 sub-funtions inside. Function "set"
## sets the matrix, "get" returns it, "setinverse" sets s as inverse fuction, 
## "getinverse" returns the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}
## The second one is about returting the inverse of matrix 'x'. 
## But not so straightforward!
## We are lazy and greedy, so, firstly, we check if we know the result already.
## (it is checked here: if(!is.null(s))) 
## If "s" is indeed not equal to NULL then we print the message 
## "getting cached data" and the result.
## If "s" is empty, then... well, no way but do the calculations :)

cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
