## The pair of functions is to cache the inverse of a matrix
## The makeCacheMatrix function has two variables: x and m. x is used to process matrix
## and m is used to stroe the result.


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

}


## The 1st step of this function is to check whether there is inversed matrix stored in cache,
## if yes, print out the message and the result. 
## if no, solve the matrix, print out the reuslt and put it into cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setmean(m)
        m
}


##example of applying the pair of functions.
matrix<-matrix(c(7,0,1,1),2,2)
cashed<-makeCacheMatrix (matrix)
cacheSolve (cashed)
cacheSolve (cashed)
