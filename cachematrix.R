#makeCacheMatrix.R
#--------------------------------------------------------
# programing assignment 2
# write two functions makeCacheMatrix() and cacheSolve()
# these are use to set or return superscoped matrices
#--------------------------------------------------------

### makeCacheMatrix function
#========================================================
# Given a matrix this function will store that matrix to
# memory outside this functions scope.This is done to reduce the inversion 
# prossing time for inverting the same matrix repeatedly.
#
# expects : an invertable matrix
# Methods : 1.getmatrix() returns the orriginal matrix  
#           2.getinnverse() returns the inverse
#
#returns : a list holding pointers to "Methods" of this function
#========================================================

makeCacheMatrix <- function(x = matrix()) {
  
  #used to store the passed matrix and it's inverse
  #if the passed matrix is noninvertible NULL is stored in "cacheinverse"
  #NOTE: this can be called from cacheSolve() to reset both stored matrices
  setinverse <-function(x){
    cachematrix <<- as.data.frame(x)  
    #try to inverse or store NULL
    tryCatch(cacheinverse <<- as.data.frame(solve(x)),
             error = function(e)cacheinverse<<- NULL)
  }
  
  setinverse(x)
  
  #used by caller to get stored inverse matrix
  getinverse <-function(){
    cacheinverse
  }
  
  #used by caller to get stored original matrix
  getmatrix <- function(){
      cachematrix
  
  }
  #return the list of named function pointers
  #setinverse() not included use makeCacheMatrix([matrix]) 
  list(getmatrix = getmatrix, getinverse = getinverse)
}


### cacheSolve function
#=====================================================
# expects   : list returned from a makeCashedMatrix call or an invertable matrix
# uses      : solve() if this is a matrix or [arg]$getinverse() for a list
# returns   : list from makeCashedMatrix for new matrix or
#             the stroed inverse for list
#
# NOTE: 1.user is resonsible for requesting a matrix stored in parent memory scope
#       2.This will overwrite the superscope matricies for a passed matrix
#=======================================================

cacheSolve <- function(x, ...) {
    if(class(x)=="list"){
      ## assume this was a returned list from makeCacheMatrix()
      #(1) does this x have a stored inverse   
      inv_x <- x$getinverse()
      
        #if the superscope inverse is not NULL return it
        if(!is.null(inv_x)){
          print("Using stored inverse")
          return(inv_x)  
        }
        #return superscope inverse 
        print("!! no superscope inverse found orinal matrix maybe noninvertable!! ")
        return(NULL)
      }
    #if the passed object is a matrix reset superscope variables
    # return function pointer list
    else{
      if(class(x)!="matrix")return(NULL)
      makeCacheMatrix(x)
    }

}