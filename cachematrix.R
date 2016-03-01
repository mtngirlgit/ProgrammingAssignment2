
## The first function sets up a special matrix object to cache the inverse of a supplied matrix.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){   ##setting objects to NULL
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve   #calculating the inverse of the matrix
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## The second function takes supplied input and compares it with the contents of the cache from the 1st function.
##  If the inverse has already been calculated and the matrix has not changed, it retrieves the contents stored
##  in the cache.  Otherwise, it calculates the inverse and replaces the cache contents.

cacheSolve <- function(x, ...) {
  m<-x$getmatrix()    ## Return a matrix that is the inverse of 'x'
  if(!is.null(m)){      ## Checking for cached data
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)   ##Calculating inverse of the matrix if cache is empty or changed.
  x$setmatrix(m)
  m
}
