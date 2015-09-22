##  In order for cacheSolve to invert matrix properly you must supply invertible matrix
##  for example: matrix(C(1,-1,1,1),2,2)

##  https://class.coursera.org/rprog-032/forum/thread?thread_id=52


##  makeCacheMatrix initialize the function invert at 
##  null then sets up 4 functions to give options to cache the matrix 
##  4 funtions (FIX, FIND, FIXINVERT, FINDINVERT) are used in List 
##  and called in CacheSolve to avoid recalculating inversion




makeCacheMatrix <- function(x = matrix()) 
{
  INVERT <- NULL
  FIX <- function (a)
  {
    x <<- a
    INVERT <<- NULL
  }
    FIND <- function ()
  {
    x
  }
    FIXINVERT <- function (INV)
  {
    INVERT <<- INV
  }
    FINDINVERT <- function () 
  {
    INVERT
  }
  
  list(FIX = FIX, FIND = FIND, FIXINVERT = FIXINVERT, FINDINVERT = FINDINVERT)
}


## cacheSolve use solve function to invert supplied matrix...
## supplied matrix comes from functions in the makeCacheMatrix portion of this function
##  if inversion has already been computed, cacheSolve will use that value and
## return text indication

cacheSolve <- function(x, ...)
{
  INVERT <- x$FINDINVERT()
  if (!is.null(INVERT)) 
  
  {
    message("!!THIS IS A CACHED VALUE!!")
    return(INVERT)
  }
  data <- x$FIND()
  INVERT <- solve(data, ...)
  x$FIXINVERT(INVERT)
  INVERT
}
