## The Pupose of makeCacheMatrix fucntion is to create the inverse of a matrix
## makeCacheMatrix fucntion creates list that can store four functions.
## namely 
## set function : keeps two values 1. value of the function(x) 2. value of teh inverse (i). Set function will assign the given matrix to the matrix variable(x) and null to the inverse variable (i)
## get fuction :  return the assgined value, if the matrix.
## setinverse function :  find out the inverse of the fucntion and assign the inverse valaue to i
## getinverse fucntion: return the inverse value (i)
## both x and i are varaibles in side the enviorenment of the makeInverse function.

makeCacheMatrix <- function(x = matrix()) {
set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(input) 
        {i <<- solve(input) }
  
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)


}


## Purpose : this function generates the inverse of he matirx that is input
## If the inverse is already there is cache it takes that
## ilse it will create the inverse and set this value to the inverse matrix variable


cacheSolve <- function(x, ...) {
         imx <- x$getinverse(x)
 ## takes teh value if its in the envioranment
  if(!is.null(mx)) {
    message("getting cached data")
  ##  print(mx)
    return(mx)
  }
 ## else generate the inverse of the matrix and set the inverse matrix variable in the enviorenmant to this value.
  data <- x$get()
  imx <- solve(data)
  x$setinverse(imx)
  return(imx)

}
