## Put comments here that give an overall description of what your
## functions do

## My makeCacheMatrix functions attempt to do the following
##1. make object with a call to makeCacheMatrix(); 
##2.calculate, store and return 'matrix' value with the first cachemean(object)
##access of the object; 
##3. fetch previously stored matrix value with the second cachemean(object)
##access of the object).

##My cacheSolve function does
##1. When called it first gets the matrix from the object x (x$getmatrix())
##2. then If there is a non-NULL value in the matrix then a message is sent
##and the matrix is returned (return(m)) and the solve function stops.


## Write a short comment describing this function

## makeCacheMatrix   creates an "object" of type 'list'. 
##This object stores two things, the original inversematrix's value
##and what will be the cached value, which is initially set to 'NULL'.
## We use makeCacheMatrix() to create an object, then access that object

makeCacheMatrix <- function(x = inv()) {
 
                 m<-NULL
         
                 setinv<-function(y){
                         x<<-y
                         m<<-NULL
                 }
                 getinv<-function() x
                 setinv<-function(solve) m<<- solve
                 getinv<-function() m
                 list(set=setinv, get=getinv,
                      setinv=setinv,
                      getinv=getinv)       
        

}


## Write a short comment describing this function
##cacheSolve() accesses the object created when makeCacheMatrix() was called
##by fetching the value of the matrix used to create the object
# this matrix being stored when the object was created

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinv()
             if(!is.null(m)){
                       message("getting cached data")
                      return(m)
               }
               matrix <- x$getinv() 
               m<-solve(inv, ...)
                x$setinv(m)
                m    
        
}

