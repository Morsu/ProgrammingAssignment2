## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix contains 4 methods:
## set ~ resembling the __init__ method from python
## getmatrix to retrieve cached matrix
## setinverse to cache inversed matrix( inverse should be computed outside of below func)
## getinverse returns cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    
    inverse<-NULL
    
    set <-function(y){
        # might be used to reset the existing object
        x<<-y
        inverse<<-NULL
    }
    
    getmatrix<-function() x   # returns cached matrix
    setinverse<-function(inv=matrix())inverse<<-inv   
    
    
    getinverse<-function() inverse
    
    # the list being returned allows to use subsetting: to invoke set one needs to exec xxx$set()
    list(set=set,getmatrix=getmatrix,setinverse=setinverse,getinverse=getinverse)

}


## uses makeCacheMatrix object to get inverse of matrix/ set inverse if not done yet

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse<-x$getinverse()
    if (!is.null(inverse)){
        message ("getting cached inverse of matrix")
        return (inverse)
    }
    matrix<-x$get()
    inverse<-solve(matrix)
    x$setinverse(inverse)
    inverse
}

mtx<-matrix(sample(1:10,size=25,replace=T) ,nrow=5,ncol=5)
cacheobj<-makeCacheMatrix(mtx)
cacheobj$getmatrix()

solve(mtx)
det(mtx)
cacheobj$setinverse(solve(mtx))
cacheobj$getinverse()
