##makeCacheMatrix is a special matrix can calculate its invertiable matrix and store it
##the MakecacheMatrix stores a list of 4 functions,
#1.set the value of the matrix: change the matrix store in the makeCacheMatrix
#2.get the value of the matrix
#3.set the value of the inverse
#4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setinverse<- function(inverse) m <<-inverse 
        getinverse<- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)              
}

##The cacheSolve function calculates the inverse of the special "matrix" created with the above function. 
##Firstly, it checks if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        m<-x$getinverse() 
        if(!is.null(m)){
         message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data, ...)
        x$setinverse(m)
        m      
}