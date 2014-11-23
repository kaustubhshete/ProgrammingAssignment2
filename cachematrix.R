## Implementing functions which will help us to cache inverse of matrix


## creating an jobject which can store its own inverse
makeCacheMatrix <- function(x = matrix()) {

  prop_inverse <- NULL
  set <- function (matrix){
    m <<- matrix
    prop_inverse <<- NULL
  }
    
  get <- function(){
    m
  }

  setInverse <- function(inverse) {
    prop_inverse <<- inverse
  }
  

  getInverse <- function() {
    prop_inverse
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##This function will get the inverse of the matrix returned by makeCacheMatrix.
cacheSolve <- function(x, ...) {
  
  matrix <- x$getInverse()
  
  if( !is.null(matrix) ) {
    return(matrix)
  }
  data <- x$get()
  matrix <- solve(data) %*% data
  x$setInverse(matrix)
  
  ## Return a matrix that is the inverse of 'x'
  matrix
}
