# R Programming - Assignment 2

# makeCacheMatrix  creates a special "matrix" object that can cache its inverse.
# cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated the inverse is retrieved from the cache.

# sample output:
# > m  <- makeCacheMatrix(matrix(c(1,2,3,4),nrow = 2,ncol = 2))
# > cacheSolve(m)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# 
# > cacheSolve(m)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5


#Create an object to get and set a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL # i will hold the inverse
  
  set <- function(y) {  
    x <<- y # x will hold the original matrix
    i <<- NULL
  }
  
  get <- function(){
    x
  }
  
  setinverse <- function(inverse){  
    i <<- inverse   
  }
  
  getinverse <- function(){
    i
  }
  
  #return list of functions  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x', use cached calue if it exists
cacheSolve <- function(x, ...) {
  
  #check if inverse already exists
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)  #return cached inverse
  }
  
  #if inverse not stored
  data <- x$get()
  m <- solve(data, ...) #calculate inverse
  x$setinverse(m) #store inverse
  m
}

