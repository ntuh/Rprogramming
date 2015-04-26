
makeCacheMatrix <- function(Matrice = matrix()) {
      invers <- NULL
      set <- function(x) {
            Matrice <<- x;
            invers <<- NULL;
      }
      
      
      get <- function() return(Matrice);
      setinv <- function(inv) invers <<- inv;
      getinv <- function() return(invers);
      return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

cacheSolve <- function(Matrice, ...) {
      invers <- Matrice$getinv()
      if(!is.null(invers)) {
            return(invers)
      }
      
      
      data <- Matrice$get()
      invserse <- solve(data, ...)
      Matrice$setinv(invers)
      return(invers)
}
