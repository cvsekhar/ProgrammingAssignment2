## makeCacheMatrix - takes a matrix and allows to cache the inverse of matrix.
## cacheSolve - takes a matrix wrapped in makeCaheMatrix and returns the 
##              inverse of matrix if present in cache or calculates the 
##              inverse of matrix if not present in the cache

## makeCacheMatrix -- takes a matrix and returns a list of functions that can
## be used to get the original matrix and inverse of the matrix.
##    get() --> returns the matrix
##    set(x) --> replaces the original matrix with the new one.
##    getInv() --> returns the inverse of matrix
##    setInv(x) --> set the inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
   invM <- NULL
   set  <- function(y){
             x <<- y
             invM <<- NULL
           }
   get  <- function() x
   setInverse <- function(i) invM <<- i
   getInverse <- function() invM
   list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve -- takes a matrix wrapped by makeCacheMatrix and calculates the
## inverse of matrix. 
## It returns the matrix inverse from the cache by the following rules:
##  if cache is not null and
##  if dimensions of matrix and inverse matrix are equal and
##  if multiplying matrix by its inverse is identity matrix
## If the above cache rules fail it calculates the inverse of matrix stores the
## results in cache and returns the inverse of matrix.
cacheSolve <- function(x, ...) {
        m <- x$get()
        im <- x$getInverse()
        if(!is.null(im)){
           if(identical(nrow(m),nrow(im)) &&
              identical(round(m %*% im), diag(nrow(m)))){
              message("Inverse of matrix found in cache")
              return(im) 
           }
        }
        im <- solve(m)
        x$setInverse(im)
        im
}
