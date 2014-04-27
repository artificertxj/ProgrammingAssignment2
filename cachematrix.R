##"cachematrix.R" function will store a solved matrix. When user calls the function and input certain matrix,
##it will first check the result function was solved before and bound with certain variable name
##if it was solved before, return the value stored with cache. If not, solve it and store it in cache

## "makeCacheMatrix.R"
## take a matrix as the argument and save it in cache

makeCacheMatrix<-function(x=matrix()){
     inv<-NULL     ##inverse of the input matrix
     set<- function(y){
             x<<-y
             inv<<-NULL
     }             ##set the matrix given by user
     get<-function() {x} ##return the matrix
     setInv<-function(Inverse) {inv<<-Inverse}  ##set the inverse matrix with solved result
     getInv<-function() {inv}  ##return the inver matrix
     list(set=set, get=get, setInv=setInv,getInv=getInv)
}



## "cacheSolve.R"
## compute the inverse matrix of a given matrix. If the result was solved before, just 
## get the value from the cache
## the argument "x" is the list returned from makeMatrix() function

cacheSolve<-function(x,...) {
  
  inv<-x$getInv() ##check if inv exsits alrdy
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }               ##inv was solved before, return the value saved 
  mtx<-x$get()    ##if inv is null, gets the original matrix and solve it
  inv<-solve(mtx)
  x$setInv(inv)
  inv
}
