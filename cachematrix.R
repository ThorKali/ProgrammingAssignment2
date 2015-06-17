## The functions store and calculate a matrix and its inverse


## store and read a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL                                
  setmatrix<-function(y){                       #function "setmatrix" with argument y 
    x<<-y                                       #store y in x
    inverse<<-NULL                              #inverse stays NULL
  }
  getmatrix<-function() x                       # function getmatrix gives value of x
  setinverse<-function(inverse) inverse<<-inverse  #set inverse to argument "inverse" of the function
  getinverse<-function() inverse                    #get "inverse"                         
  list(setmatrix=setmatrix, getmatrix=getmatrix,    # store values
       setinverse=setinverse,
       getinverse=getinverse)
}


## search the inverse of a matrix in memory. If not in memory, calculate the inverse and store in 'inverse'

cacheSolve <- function(x, ...) {
  inverse<-x$getinverse()                    #get inverse stored in memory
  if(!is.null(inverse)){                     #if inverse is not NULL
    message("getting cached data")           #print message and the stored inverse
    return(inverse)
  }
  matrix<-x$getmatrix()                       #if inverse is not stored, calculate inverse and store into 'inverse'
  inverse<-solve(matrix, ...)
  x$setinverse(inverse)
  inverse
}









