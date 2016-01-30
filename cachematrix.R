## based on code supplied for Assignment 2
## makeCacheMatrix creates a special matrix object that can cache its inverse
## assumes matrix supplied is always invertible

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {  ## changes values of x and i stored in makeCacheMatrix (main function)
    x <<- y  ## substitutes x with y (the input) in makeCacheMatrix; x <- y would substitute x with y only in set function
    i <<- NULL  ## restore i to value of null, because previous value of previous matrix not needed anymore. Need to recalculate i through cacheSolve
  }
  get <- function() x  ## returns x stored in makeCacheMatrix
  setinverse <- function(inverse) i <<- inverse  ## store value of input (which is NOT inverse of x) in variable i into makeCacheMatrix
  getinverse <- function() i  ## returns i
  list(set = set, get = get,  ## returns list
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special matrix created in makeCacheMatrix
## it first checks to see if the inverse has already been calculated
## if so, it gets the inverse from the cache and skips the computation
## otherwise it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function
## Package MASS must be installed to run this function: install.packages("MASS")
## if matrix was always square, solve() could be used instead of ginv() & wouldn't need MASS package

cacheSolve <- function(x, ...) {  ## input is the object where makeCacheMatrix is stored
  library(MASS)  ## load MASS package, which is required for ginv() function
  i <- x$getinverse()  ## calls getinverse() within x's environment by subsetting the getinverse function	
  if(!is.null(i)) {  ## if i exists and is not NULL
    message("getting cached data")	
    return(i) ## returns cached i
  }  ## equivalent of "else": this is what happens if i doesn't exist or is NULL
  data <- x$get()  ## gets matrix stored with makeCacheMatrix		
  i <- ginv(data, ...)  ## ginv() calculates Moore-Penrose generalized inverse of matrix
  x$setinverse(i)  ## stores inverse in object generated with makeCacheMatrix
  i  ## returns a matrix that is the inverse of x
}


## TESTING IF FUNCTIONS WORK
## Reference for m1 is https://www.mathsisfun.com/algebra/matrix-inverse.html
## Create 2 x 2 matrix called m1
m1 <- c(4,2,7,6)  ## vector; as matrix 1st column = 4 & 2, 2nd column = 7 & 6; 1st row 4 & 7, 2nd row 2 & 6
dim(m1) <- c(2,2)  ## turn into matrix by specifying dimensions 2 x 2
## inverse will be 2 x 2 matrix with 1st column = 0.6 & -0.2, 2nd column = -0.7 & 0.4; 1st row 0.6 & -0.7, 2nd row -2 & 0.4
r1 <- makeCacheMatrix(m1)
cacheSolve(r1) ## calculates and returns inverse of m1
cacheSolve(r1) ## inverse is returned from cache because it's already been calculated
## test with second matrix that isn't square (3 x 2)
m2 <- c(5,3,4,9,1,2)
dim(m2) <- c(3,2)
r2 <- makeCacheMatrix(m2)
cacheSolve(r2)
cacheSolve(r2) ## inverse is returned from cache because it's already been calculated
cacheSolve(r1) ## still returns value in cache for r1
