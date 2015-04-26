# From swirl: "You can pass functions as arguments to other functions just like you can pass
# data to functions."
# This is basically what is done in this scribt, by first making a matrix in the first function,
# which is then used in  cacheSolve function afterwards

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

#we see that the matrix "matrice" is passed on to the second function

# In the following function, notice the elipis as te last argument.
# from swirl: "The ellipses can be used to pass on arguments to other functions that are
# used within the function you're writing. Usually a function that has the 
# ellipses as an argument has the ellipses as the last argument."

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
