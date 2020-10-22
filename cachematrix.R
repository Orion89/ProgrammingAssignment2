## In this script there are two functions.
## makeCacheMatrix creates a special "matrix" that can cache its inverse, so as not to 
## recalculate the inverse in case it has already been determined.
## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated, then this fucntion retrieve the inverse 
## from the cache.



## makeCacheMatrix function creates a special "matrix" object that can cache its inverse. 
## Actually, it's a list of functions that perform the following functions
## 1.set the values of the matrix
## 2. get the values of the matrix
## 3. checks if the matrix is square
## 4. set the inverse of the matrix
## 5. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv_mat <- NULL
  
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  
  is_square <- function(){
    dimen <- dim(x);
    if (dimen[1] != dimen[2]){
      FALSE;
      print("The matrix must be square");
    }
    else TRUE;
  }
  
  get_matrix <- function() x
  set_inv <- function(inv) inv_mat <<- inv # set the inv matrix calculated to the inv_mat variable
  get_inv <- function() inv_mat # get the inv_mat
  
  
  # return the list object
  list(set = set, get_matrix = get_matrix,
       set_inv = set_inv,
       get_inv = get_inv,
       is_square = is_square)

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above,if the matrix is square. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inv()
  
  if(!is.null(inv)) {
    message("getting cached data: inv matrix")
    return(inv)
  }
  
  m_square <- x$is_square()
  
  if (m_square == TRUE){
    matrixx <- x$get_matrix()
    inv <- solve(matrixx, ...)
    x$set_inv(inv)
    inv
  }
  else print("the matrix is not square. Please enter a square matrix")
}
