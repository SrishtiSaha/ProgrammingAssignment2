##Matrix Inversion is usually a costly computation 
## following 2 functions are used to cache the inverse of a matrix

#Caching the code for matrix inversion
#makeCacheMatrix creates a list containing a function to:
##1.) set the value of the matrix
##2.) get the value of the matrix
##3.) set the value of inverse of the matrix
##4.) get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {

#initialize the inverse property
inv<-NULL

##method to set the matrix
set <- function(y){
x<<-y
inv<<-NULL
}

##method to get the matrix
get<-function()x

##method to set the inverse of matrix
setInverse <- function (inverse) inv<<- inverse

##method to get the matrix
getInverse <- function() inv

##return the list of methods
list(set=set, get=get,
	setInverse=setInverse,
	getInverse=getInverse)


}



##this function will compute the inverse of the matrix created by makeCacheMatrix##if the inverse has already been computed. and the matrix has not changed,
##this function would retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv<- x$getInverse()
		
		#for cases when inverse has already been computed
		if(!is.null(inv)){
		message("retrieving cached data")
		return(inv)
		}
		
		#get matrix from the object
		mat<-x$get()
		
		#calculate inverse using matrix multiplication
		inv<- solve(mat, ...)
		
		#set inverse to object
		x$setInverse(inv)
		
		#return the matrix
		inv
}
