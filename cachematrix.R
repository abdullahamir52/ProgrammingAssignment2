# A function that cache inverse of a given matrix

makeCacheMatrix <- function( mat = matrix() ) 
{

  initial <- NULL
  set <- function(matrix) 
  {
    mat <<- matrix
    initial <<- NULL
  }
  
  get <- function() 
  {
    mat
  }
  
  setInv <- function(Inv) 
  {
    initial <<- Inv
  }
  
  getInv <- function() 
  {
    initial
  }
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

# returns the inverse of the matrix gives by abovementioned matrix

cacheSolve <- function(x, ...) 
{

  mat <- x$getInv()
  
  if( !is.null(mat) ) 
  {
    message("getting cached data")
    return(mat)
  }
  
  data <- x$get()
  
  mat <- solve(data) %*% data
  
  x$setInv(mat)
  
  mat
}