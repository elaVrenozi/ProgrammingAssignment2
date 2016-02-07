## Caching the Inverse of a Matrix:
## - As specified on the project request the matrix inversion is usually a costly computation
## so there may be some  benefit of caching the inverse of a matrix rather than compute it over and over
## Below are the functions that i used for this purpose, the first creates the matrix and stores it on the cache 
##than the second seeks if the inverse of  the matrix is already on the cache and is not changed it retrives it 
##otherwise it computes the nverse of the matrix created by the first one.


## This is the first function which creates a special "matrix" object that can cache its inverse.
## first the matrix is created than is calculated the inverse of the matrix (m) using the solve() function
##and stores the inverse on the cache via setmatrix() function
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
  x<<-y
  m<<-NULL
}
get<-function() x
setmatrix<-function(solve) m<<- solve
getmatrix<-function() m
list(set=set, 
     get=get,
     setmatrix=setmatrix,
     getmatrix=getmatrix)
}

 
## Than this second function computes the inverse of the matrix
## created by the first makeCacheMatrix function above. 
## First it checks if the inverse has already been calculated (and the 
## matrix has not changed), if so it gets the result from the cache and skips the rest of the computation.
## If the inverse of the matrix isnt on the cache than it computes the inverse of the matrix, sets the value in the cache via
## setmatrix function. the function takes always an invertible matrix as an argument

cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
      message("get data that are cached previously")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}



## an example of a sample run
## > getwd()
## [1] "C:/Users/Ela/Desktop/ProgrammingAssignment2"
## > source("cachematrix.R")
## i use the makeCacheMatrix to create the matrix object
## > orig_matrix<-makeCacheMatrix(matrix(1:4, 2, 2))
## > orig_matrix$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4

## than i call the cacheSolve, no inverted matrix is on the cache on the first run
## > cacheSolve(orig_matrix)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## when called for the second time it retrives the inverse from the cache
## > cacheSolve(orig_matrix)
## get data that are cached previously
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5