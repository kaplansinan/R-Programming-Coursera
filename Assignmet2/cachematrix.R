## The following functions work together to create a square invertible matrix
## and make the inverse of the matrix available in the cache environment
## to reduce computational time

## makeCacheMatrix creates and returns a list of functions which are
## used by cacheSolve to get or set the inverse of a matrix in cache
makeCacheMatrix <- function(x = matrix()) {
        ## store the cached value and assign NULL
        cache <- NULL
        ## set a new value to x
        set <- function(y){
                x <<-y
                cache <<-NULL
        }
        ## get the value of x
        get <- function(){
                return (x)
        } 
        ## set inverse of the matrix
        setInverse <- function(inverse){
                cache <<- inverse
        }
        ## get the inverted matrix
        getInverse <- function(){
                return (cache)
        }
        
        # return the above functions to the working environment
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix function and
## return a matrix that is the inverse of 'x'
## If the inverse of the matrix does not exist in cache, it is created in 
## the working environment and the inverse of the matrix  is stored in cache
cacheSolve <- function(x, ...) {
        ## try to get the inverse of the matrix stored in cache
        cache <- x$getInverse()
        
        # return inverse of the matrix from cache if it exists
        # else create the matrix in working environment
        if (!is.null(cache)) {
                message("getting inverse of the matrix in the cache")
                
                # display matrix in console
                return(cache)
        }
        
        # create matrix since it does not exist
        matrix <- x$get()
        
        # make sure matrix is square and invertible
        # if not, handle exception cleanly
        tryCatch( {
                # inverse of matrix
                cache <- solve(matrix, ...)
        },
        error = function(e) {
                message("Error occuured, which is:")
                message(e)
                
                return(NA)
        },
        warning = function(e) {
                message("Warning:")
                message(e)
                
                return(NA)
        },
        finally = {
                # set inverse of the matrix in cache
                x$setInverse(cache)
        } ) ## end of tryCatch statement
        
        # return inverted matrix
        return (cache)
}
## sample run 
## 1-) source("cachematrix.R")    load R program
## 2-) x = rbind(c(1, 3), c(2, 4)) create data
## 3-) m <- makeCacheMatrix()     create functions
## 4-) m$set(x)  create matrix in working environment
## 5-) cacheSolve(a)              1st run returns inverted matrix
##                              from working environment
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## 6-) cacheSolve(a)              2nd and subsequent runs
##                              returns inverted matrix from cache
## getting inverse of the matrix in the cache        
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
