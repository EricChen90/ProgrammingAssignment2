## The first function (makeCacheMatrix) save the matrix into cache
## The second (cacheSolve) will compute the inverse of the matrix that
## has been saved by makeCacheMatrix, and save the result to the cache,
## if the same matrix that has been computed by cacheSolve passed into the function
## the cacheSolve will get the cached data instead of computing
## the inverse of the matrix using solve function again which can saves time
## if working using a large matrix repeatedly.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## it also stores functions used in second function (set, get, setinv, getinv)
## so the functions can be used by cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
        ## setting m(cache) to NULL 
      m <- NULL
        ## set can be used for changing matrix stored in main function
        ## not necessary used if do not want to change the matrix
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
        ## get the matrix from the input
      get <- function() x
        ## save the value of the input to m
      setinv <- function(solve) m <<- solve
        ## get data stored in m
      getinv <- function() m
        ## save the function as a list
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
        ## getting data stored in m
      m <- x$getinv()
        ## If m is not null (inversed matrix has been saved to cache) it will
        ## print the message and print the inverted matrix
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
        ## else (no data stored in m) it will get the data from the matrix
        ## then use solve function to the matrix saved by makeCacheMatrix.
        ## saving the inverted matrix to m, save it to cache using x$setinv(m)
        ## and print m (the inverted matrix)
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
