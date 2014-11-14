## This file contains functions for Programing Assignment 2 of the R Programming
# course on Coursera. These functions can be used to calculate the inverse of a 
# square matrix (assumed non-singular for simplicity).


## This function returns a list of functions to set/get a given matrix, 
# and to get/set its inverse. The implementation below was based on the example 
# function 'makeVector', which is available from the description of 
# Programming Assignment 2
makeCacheMatrix <- function(new.M = matrix()) {
  if(ncol(new.M)!=nrow(new.M)){
    stop("Non-square matrices are not valid for this function.")
  }
  M.inv <- NULL
  M<-new.M
  set.M <- function(new.M) {
    if(ncol(new.M)!=nrow(new.M)){
      stop("Non-square matrices are not valid for this function.")
    }
    M <<- new.M
    M.inv <<- NULL
  }
  get.M <- function() M
  set.inv <- function(new.inv) M.inv <<- new.inv
  get.inv <- function() M.inv
  list(set.M = set.M, get.M = get.M,
       set.inv = set.inv,
       get.inv = get.inv)
}


## This function is used to return the inverse of a (square) matrix M. If this
# inverse has already been evaluated it returns the stored value, otherwise it
# evaluates the inverse, stores it in the 'M.inv' variable, and then returns the
# value.
cacheSolve <- function(M, ...) {
        ## Return a matrix that is the inverse of 'M'
    M.inv<-M$get.inv()
    if (is.null(M.inv)){
      M.inv<-solve(M$get.M())
      M$set.inv(M.inv)
    } else {
      message("Cached [M]^{-1} retrieved.")
    }
    return(M.inv)
}

## Test performed (single run, not a scientific evaluation): 
# Mac Mini (late 2012); 2.6GHz Intel core i7; 8Gb 1600 MHz DDR3
#
# > a<-makeCacheMatrix(matrix(runif(2500^2),2500,2500))
# > init.t<-Sys.time(); b<-cacheSolve(a); end.t<-Sys.time()
# > end.t - init.t
# Time difference of 16.2028 secs
# >
# > init.t<-Sys.time(); c<-cacheSolve(a); end.t<-Sys.time()
# Cached [M]^{-1} retrieved.
# > end.t - init.t
# Time difference of 0.0007681847 secs
# >
# > identical(b,c)
# [1] TRUE