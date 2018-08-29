## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## I could not have done this without finding solutions on
## the Internet. I had approached it differently for several days
## based on different interpretations of the problem. 
## Plus I would not have figured out how to do several of these steps.

## This creates a square matrix called z to test the function;
## default 100 x 100; all random normal, mean=0, sd=1

makematrix<-function(n=100){

  z<-rnorm(n^2)
  dim(z)<-c(n,n)

  # puts z to the parent environment; 
  # n<<-n; I do not need to put n to the parent environment
  z<<-z
}

makeCacheMatrix <- function(x = matrix()) {

  # x is a matrix, entered by name of the matrix
  # with the function I created, the new matrix will be called z

## This part not needed, and removed from the logic   
  # Create a matrix for the Inverted Matrix, same size as
  # original square matrix; put in numbers that will change
  
  #  I cannot make a matrix full of NULL entries. I tried.
  #  Null is Null. 
  #  Plus tests: inv=NULL, this is an indicator to test later.
 
 inv <-NULL
  
  # inv will become the Inverse of the loaded Matrix 
  
  # Create function to move results to global environment
  # this moves both matrix named x, and inv to parent environment
 
  set <- function (y){
    
      x<<-y
      inv<<-NULL
 
  } 
 
  # not quite sure why this works, with x on outside  
  get <- function() x
  
  # copies to global environment; code on the same line(?)
  setinv<-function(inverse) inv <<-inverse

  # This gets the inv matrix
  getinv <- function() inv
  
  # This creates a list, a basket of stuff
  # I tried ## the line below (removing), and functions did not work
  # This allows me to later refer to funtions with the $ operator
  # (I'm not sure why this works)
  
  a<-list(set=set, get=get, setinv=setinv, getinv=getinv)
#  a<<-a
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## the ... means other attributes carried forward
  
  inv<-x$getinv()
  
  # test to see if the inverse has already been calculated
  # is inv not equal to null (meaning, has it been calculated?)
  # If yes, proceed
  
  if(!is.null(inv)){
    
  # if it has been calculated, get it from the cache
  
  print("Getting from the Cached Data")  
    return(inv)
  }
  
  # If not in cached data, meaning a new matrix, 
  # then calculate the inverse
  
  # mat.data<-x$get()
  # inv<-solve(mat.data, ...)
  
  # I had put z (the new matrix) into the parent environment
    inv<-solve(z)
  
  # puts the value of the inverse in the cache
  # through the MakeInv function
  
  # x$setinv(inv)
  
  return(inv)
  }

## To test these; first it does for a new matrix and calculates
## the inverse
## then with the inverse already calculated, will call from cache.

## For smaller matrices, it is quicker to calculate then call
## from cache. Break even at about 65 x 65

test <-function(mat){
   ## mat is an invertible matrix
   temp<-makeCacheMatrix(mat)

   start.time<- Sys.time()
   cacheSolve(temp)
   dur1=Sys.time() - start.time
   # takine a different in time automatically added
   # Time difference of __ secs
    print("Time for Original Calculation")
    print(dur1)
    print("")
    
   start.time<-Sys.time()
   cacheSolve(temp)
   dur2<-Sys.time()- start.time
  
   print(dur2)
   print("")
  
   print("Time saved by Call from Cache")
   print(dur1 - dur2)
}
