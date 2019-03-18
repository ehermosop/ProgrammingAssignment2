#In this example we introduce the <<- operator which can be used to assign a value to an object in an environment that is 
#different from the current environment. Below are two functions that are used to create a special object that stores a 
#numeric vector and cache's its mean.

#The first function, makeVector creates a special "vector", which is really a list containing a function to

#set the value of the vector
#get the value of the vector
#set the value of the mean
#get the value of the mean

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

#The following function calculates the mean of the special "vector" created with the above function. However,
#it first checks to see if the mean has already been calculated. If so, it gets the mean from the cache and skips
#the computation. Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via 
#the setmean function.

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

###############################################################################################
## according to the instructions this part of the code creates an invertible special matrix  ##
## This function creates a special "matrix" object that can cache its inverse.               ##
###############################################################################################
##                This space contains the edited code of the assignment.                     ##
###############################################################################################

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
      set <- function(y) {
          x <<- y
   inv <<- NULL
      }
      
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

####################################################################################################
##  Below is the function that calculates the inverse of the matrix created by "MakeCacheMatrix"  ##
##  from the previous section. Under the assumption of a well-calculated "inverse", the result    ##
##  must produce the inverse of the cache.                                                        ##
####################################################################################################

cacheSolve <- function(x, ...) {
  ##################################################################
  # At this point the matrix that is inverse of "X" is recovered.  #
  ##################################################################
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

#######################################
##        Checking the code           #
#######################################

 m <- matrix(rnorm(25),5,5)
 m1 <- makeCacheMatrix(m)
 cacheSolve(m1)

############################### 
# One of the possible result  #
###############################
  
# [,1]       [,2]       [,3]         [,4]       [,5]
# [1,]  0.3362873 -0.2873034 -0.5560245 -0.001058057 -0.6136569
# [2,] -1.5746053  1.9414651  0.7913718  0.168165917  2.2100358
# [3,] -2.6753914  2.3545532  1.3149956 -0.131643120 -0.5645091
# [4,]  0.7954410 -1.3602708 -0.3731967  0.110646247 -0.2363929
# [5,] -0.6465791  0.9985758  0.5103608  0.264314180  0.7935331
 