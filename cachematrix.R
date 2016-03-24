makeMatrix <- function(x = matrix())
{
  slv <- NULL  ##variable for caching Solve() result
  
  ##function, setting new matrix value:
  set <- function(y)
  {
    x <<- y
    slv <<- NULL
  }
  
  ##function, which returns matrix:
  get <- function() x
  
  ##function, setting new slv value:
  setslv <- function(new_val) slv <<- new_val
  ##function, which returns slv value:
  getslv <- function() slv
  
  list(set = set, get = get,
       setslv = setslv,
       getslv = getslv)
}

cacheSolve <- function(x, ...) {
  ##getting old slv value
  slv <- x$getslv()
  ##checking if old cached value of slv is actual and returning it, if it is.
  if(!is.null(slv))
  {
    message("getting cached data")
    return(slv)
  }
  
  data <- x$get()
  ##calculating new value to cache
  slv <- solve(data, ...)
  ##updating slv
  x$setslv(slv)
  slv
}