#makeCacheMatrix takes a matrix and stores its inverse once it has been calculated
#Cachesolve first checks to see if an inverse has been stored in makeCacheMatrix, 
# and if not, calculates the inverse of the matrix using the solve function. 

#makeCacheMatrix requires a square matrix. See example:
# mat<- matrix(c(1,1,2,2),2,2)
# x <-makeCacheMatrix()
# x$set(mat) would input matrix that would have cached inverse 
# x$get() would print matrix with cached inverse 
# x$setinverse() would set inverse of cached matrix (which links up to Cachesolve)
# x$getinverse() would retrieve inverse of cached matrix (which also links up to Cachesolve)

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get, 
           setinverse = setinverse,
           getinverse = getinverse)
}

#Cachesolve checks to see if inverse of cached matrix specified in makeCacheMatrix 
# has been calculated
# If getinverse is already populated it retrieves from makeCacheMatrix function
# Otherwise it calculates the inverse of the matrix using the solve function, and 
# Returns result to setinverse. 
# out <- cacheSolve(x)

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
