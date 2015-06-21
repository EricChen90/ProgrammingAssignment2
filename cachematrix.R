## The first function (makeCacheMatrix) save the matrix into cache
## The second (cacheSolve) will compute the inverse of the matrix that
## has been saved by makeCacheMatrix, and save the result to the cache,
## if the same matrix that has been computed by cacheSolve passed into the function
## the cacheSolve will get the cached data instead of computing
## the inverse of the matrix using solve function again which can saves time
## if working using a large matrix repeatedly.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## setting m(cache) to NULL 
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(solve) m <<- solve
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}

## The second function cacheSolve
## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
        ## If m is not null (inversed matrix has been saved to cache) it will
        ## print the message and print the inversed matrix
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
        ## else (no data stored in m) it will get the data from the matrix
        ## and use solve function to the matrix saved by makeCacheMatrix
        ## and print the inverted matrix
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
}

## Checking the function:
## 1. create the invertible matrix
## 2. pass the matrix into makeCacheMatrix and assign it to another values
## 3. pass the value created in step 2 into cacheSolve function
##    cacheSolve will compute and print the inverse of the matrix
## 4. using the same value created in step 2 to cacheSolve function will 
##    give you "getting cached data" message and print the inverse of the matrix
