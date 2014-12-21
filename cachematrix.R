## This function creates a special "matrix" object that can cache its inverse
## It calculates the inverse of the matrix if it is not already calculated. 
## In case it is already calculated the function returnes the chached value.

makeCacheMatrix <- function(x = matrix()) 
{
        inverseX <- NULL
        set <- function(y) 
        {
                x <<- y
                inverseX <<- NULL
        }
        get <- function() 
        {      
                x
        }
        setinverse<- function(inverse) 
        {
                inverseX <<-inverse
        }
        getinverse <- function()
        {
                inverseX
        }
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
        inverseX <- x$getinverse()
        if (!is.null(inverseX)) 
        {
                message("Get the cached inverse matrix")
                return(inverseX)
        } 
        else 
        {
                inverseX <- solve(x$get())
                x$setinverse(inverseX)
                return(inverseX)
        }
}
