#The makeCacheMatrix will store the matrix in cache


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #will make it able to take the inverse
  m <- NULL    #m is the object created
  set<- function(y){  #sets the matrix
    x <<- y
    m <<- NULL
  }
  get <- function()x    #gets the matrix
  setinverse <- function(inverse) m <<-  inverse #sets the inverse
  getinverse <- function() m   #gets the inverse function
  list( set = set, get = get, setinverse= setinverse, getinverse = getinverse)

}


## Write a short comment describing this function
#NOW to return the inverse of a matrix
cacheSolve <- function(x, ...) {
      
  m <- x$getinverse()  #x is assigned to m (cache) and returned 
  if(!is.null(m)){
    message("getting chached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
#ExampleOutput
#x = rbind(c(1, -1/4), c(-1/4, 1))
#m = makeCacheMatrix(x)
#m$get()
#     [,1]  [,2]
#[1,]  1.00 -0.25
#[2,] -0.25  1.00

#cacheSolve(m)
#        [ ,1]      [,2]
#[1,] 1.0666667 0.2666667
#[2,] 0.2666667 1.0666667

#cacheSolve(m)
#getting chached data
#     [,1]      [,2]
#[1,] 1.0666667 0.2666667
#[2,] 0.2666667 1.0666667