##The following function creates a special "matrix" object
##that can cache its inverse

##Saving information in cache could be useful for complex or time-consuming
##operations, since it facilitates getting this information later without
##having to run the function and calculations all over again.

makeCacheMatrix <- function (x = matrix()){ #x is a matrix
  inv <- NULL #inv is a variable assigned null object (in parent function)
  set <- function(y){
    x <<- y #assign value to the matrix (in child function)
    inv <<- NULL
  }
  get <- function() {x} #get value of matrix
  setInverse<- function(inverse)#set value of the inverse
  {inv<<-inverse} 
  getInverse <- function () {inv}#get value of the inverse
  list( set= set, get = get, setInverse = setInverse, getInverse = getInverse)
  #create a list for the above with value's names for later use
}

##The next function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix. If the inverse has already been calculated (and the matrix
##has not changed), this function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv<- x$getInverse() #return a matrix inverse of x, assign it to inv
  if(!is.null(inv)){ #check if the inverse is already calculated
    message("getting cached data")#if it exist, display the message
    return(inv)#and return the inverse
  }
  mat <- x$get()#if it doesn't exist, get a matrix
  inv <- solve(mat, ...)#calculate the inverse
  x$setInverse(inv)#set it
  inv #and print it!
}
