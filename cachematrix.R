## Write a short comment describing this function
## Make cacheMatrix
makeCacheMatrix <- function(x = matrix()) {
I = NULL
  get = function(){
    x
  }
  set = function(y){
    x <<- y
    m <<- NULL
  }
  setInverse = function(inverse){
    I <<- solve(x)
  }
  getInverse = function(){
    I
  }
  list(get = get, set = set, setInverse = setInverse, getInverse = getInverse)
}


## CacheSolve calculate the Inverse of matrix cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		  I = x$getInverse()
  if(!is.null(I)){
    message("getting cached data")
    return(I)
  }
  data = x$get()
  I = solve(data,...)
  x$setInverse(I)
  I
}
