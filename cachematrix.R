#Cache the inverse of a matrix because there are many computations involved 
# so this way computations wont be done repeatedly if already done.
# the below two fuctions are used to create an object to store the matrix and caches its inverse
# as a result when the inverse of a matrix is computet it wont be computed repeatedly it will be cached instead

makeCacheMatrix <- function(x = matrix()){
   m<- NULL
   set <- function(y){
     x<<- y
     m <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse) m <<- inverse
   getinverse <- function() m
   list( set = set, get = get, setinverse = setinverse, getinverse=getinverse)
}


# function that computes the inverse of a matrix only if the inverse has not been calculated previously
# elsewise, ie. if it was computed previously, the inverse will be taken from the cache
cacheSolve <- function(x, ...){
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
