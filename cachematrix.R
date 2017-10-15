## We store the inverse of the matrix in the cache:
## We keep the inverse matrix in the cache to avoid having to calculate it again and again.
## Then create two functions that are used to create an object to store the matrix and cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
 inver <- NULL
   set <- function(y) {
   x <<- y
   inver <<- NULL
   }
   get <- function() x
   setInverse <- function(inverse) inver <<- inverse
   getInverse <- function() inver
   list(set = set,
   get = get,
   setInverse = setInverse,
   getInverse = getInverse)
}


## With this function we calculate the inverse of the special matrix,
## which was created earlier by the makeCacheMatrix function.
## Verify if the already is calculated the inverse to the matrix,
## if the answer is positive obtains that information.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 inver <- x$getInverse()
 if (!is.null(inver)) {
 message("Take cached data")
       return(inver)
 }
 mat <- x$get()
 inver <- solve(mat, ...)
 x$setInverse(inver)
 inver
}
