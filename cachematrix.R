######################## Assignment 2 Rprogramming ######################


########################## Function 1 #########################

# create 1st function

makeCacheMatrix <- function (x=matrix()) {
  inv <- NULL
  set <- function(y) { # set the value of the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x # get the value of the matrix
  setinv <- function(inverse) inv <<- inverse # set the value of the inverse
  getinv <- function() inv # get the value of the inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


########################## Function 2 #########################
# create 2nd function

cacheSolve <- function(x, ...) {
  inv <- x$getinv() # check if the x$getinv is null
  if(!is.null(inv)) { # if it is not null it prints the message 'getting cached data'
    message("getting cached data")
    return(inv)
  }
  data <- x$get() # create a temporary data object
  inv <- solve(data, ...) # find the inverse of the matrix
  x$setinv(inv) # set the value of the inverse matrix
  inv # return the result
}


########## EXAMPLE : create 3 diffent types of square matrices #########
         # one 4x4 
         # one 3x3 
         # one 5x5

a <- matrix(c(1,3,4,3,2,5,6,7,3),3)
b <- matrix(c(1,3,4,3,2,5,6,7,8,9,5,4,6,6,3,3),4)
c <- matrix(c(3,2,3,5,6,8,0,3,2,2,4,6,8,9,0,5,3,4,3,4,6,3,2,2,5),5)

# create 3 new objects for the 3 square matrices by calling the function makeCachematrix


d <- makeCacheMatrix(a)
e <- makeCacheMatrix(b)
f <- makeCacheMatrix(c)

# call the cacheSolve fucntion to get the results

print('Results of Inversed Table a Below')
cacheSolve(d)
print('Results of Inversed Table b Below')
cacheSolve(e)
print('Results of Inversed Table c Below')
cacheSolve(f)


# call one of the three function to check if the message 'getting cahced data'
# appear on the console if one of the tables is already cached

cacheSolve(d)

# COPY PASTE THE CODE AND RUN TO TEST!!!
