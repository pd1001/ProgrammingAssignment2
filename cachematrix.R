## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## There are two function makeCacheMatrix,makeCacheMatrix
## makeCacheMatrix consist of set,get,setinv,getinv
makeCacheMatrix <- function(x = matrix()){ 
  j <- NULL #initial inverse taken as NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x     #This function is to get matrix x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function()j  #To obtain inverse of matrix
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
    }

 


## Write a short comment describing this function
##This is used to get cache data
cacheSolve <- function(x, ...) {   ##get cache data 
 
  j <- x$getInverse()
  if(!is.null(j)){              ##checking if inverse is NULL or not 
    message("getting cached data")
    return(j)                 ##getting inverse value
  }
  mat <- x$get() 
  j <- solve(mat,...)         #calculating inverse value 
  x$setInverse(j)
  j                          ## Return a matrix that is the inverse of 'x'
}
       
    
