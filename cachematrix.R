## These two functions make the repetitive computation of matrix inversion
##more effective, by caching the once calculated matrix inverse. The first
##function provides the caching functionality, the second itneracts with the
##cache.

## This functions initiates a cache of a matrix and its inverse. It returns a list of four functions:
## - set (caches the matrix)
## - get (retrives the matrix)
## - setinv (caches the inverse)
## - getinv (retrieves the inverse)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                   #set an empty cache for the inverse
  set <- function(y) {        #cache the matrix and empty the inverse cache
       x <<- y                
       m <<- NULL
    }
  get <- function() x                #retrieve the cached matrix
  setinv <- function(inv) m <<- inv  #cache a calculated inverse
  getinv <- function() m             #retrieve the cached inverse
  list(set = set, get = get, setinv = setinv, getinv = getinv) #return list
}


## This function interacts with the makeCacheMatrix function. It cecks, if a
## cached version of the matrix inverse exists; if yes it retrieves it, if
## not it calculates the inverse and caches it.

cacheSolve <- function(x, ...) {
  m <- x$getinv()                   #retrieve the inverse cache
  if(!is.null(m)) {                 #return the inverse if available 
    message("getting cached data")
    return(m)
  }
  data <- x$get()                   #get the matrix
  m <- solve(data, ...)             #create an inverse
  x$setinv(m)                       #cache it
  m                                 #return the inverse
}
