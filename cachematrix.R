## Put comments here that give an overall description of what your functions do

# from the example, i changed something
# 1. x = numeric() ==> m = matrix()
# 2. we wanna get a inverse of matrix instead of a mean of vector
# so, i changed the name of variable : m(mean) ==> inv(inverse)
# 3. also, i used the 'solve' function instead of 'mean' function'
# to get a inverse of m

makeCacheMatrix <- function(m = matrix()) {
    inv <- NULL
    set <- function(y) {
        m <<- y
        inv <<- NULL
    }
    get <- function() m
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)

    ## Return a matrix that is the inverse of 'x'
    inv
}