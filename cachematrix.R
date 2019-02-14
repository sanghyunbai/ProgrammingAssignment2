## Coursera Data Science Specialization, Module2 R programming week 3 assignment
## Assginment description:
# Write functions that cache the inverse of a matrix

# Background review: Lexical Scoping (its a defined rule how R search the value of a symbol)
# In other words, the values of free variables are searched for in the environment in which the function was defined
# An environment is a collection of (symbol, value) pairs
# A function, together with an environment, makes up what is called a closure or function closure
# - R programming for Data Science by Roger D. Peng
# Inverse of a squarematrix can be done by calling solve() function.
# If X is a square invertible matirx, then Y <- solve(X) to store inverted matrix at Y.

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  #"set" function to change global matrix x to given argument y 
  set <- function(y){ 
      x<<- y # change global matrix x to y 
      inverseMatrix <<-NULL # changed matrix should have different inverse matrix, so reset.
  }
  get <- function() x 
  
  setInverseMatrix <- function(inverse) inverseMatrix <<-inverse   
  getInverseMatrix <- function() inverseMatrix  
  list(set = set, get = get, 
        setInverseMatrix = setInverseMatrix, 
        getInverseMatrix = getInverseMatrix )
}
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix()
## If the inverse has already been calculated(not updated),then the cacheSolve
## should RETREIVE the inverse FROM the CACHE
cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverseMatrix()
  if(!is.null(inverseMatrix)){
      message("Cache hit")
      return(inverseMatrix)
  }
  message("Cache miss")
  data <- x$get()
  inverseMatrix<- solve(data, ...)
  x$setInverseMatrix(inverseMatrix)
  inverseMatrix
}

##Testing makeCacheMatrix()
    #Check make matrix
    a <- makeCacheMatrix(matrix(1:9,3,3))
    #Check get()
    print(a$get())
    #Check if getInverseMatrix() initially returns null
    print(a$getInverseMatrix())
    #Check set()
    a$set(matrix(c(5, 1, 0,
                   3,-1, 2,
                   4, 0,-1), 3,3))
    #check setInverseMatrix()
    k<-solve(a$get())
    a$setInverseMatrix(k)
    print(a$getInverseMatrix())

##Testing cacheSolve()  
    b <- makeCacheMatrix(matrix(1:4,2,2))    
    #Should be cache miss
    print(cacheSolve(b))
    #Should be hit
    print(cacheSolve(b))