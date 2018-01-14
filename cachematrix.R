## calculate matrix invesion is computationally heavy
## so, keeping the calculated inversed matrix and retrieve that 
## when we need, could decrease computational time


## This function create a vector that its members are functions: get, set, getInversedMAtrix, setInversedMatrix

makeCacheMatrix <- function(x = matrix()) 
{
  	inversedMatrix <- NULL
  	set <- function(y)
  	{
    		x <<- y
   		 inversedMatrix <<- NULL
  	}
  	get <- function() x
  	setInversedMatrix <- function(inversed)
 	{
    		inversedMatrix <<- inversed
  	}
  	getInversedMatrix <- function() inversedMatrix 
  	list(set = set, get = get, setInversedMatrix = setInversedMatrix,  getInversedMatrix = getInversedMatrix)
}


## This method is used to Calculate inversed matrix if inversedMatrix is NULL
##else it would retrieve inversedMatrix without need to calculation

cacheSolve <- function(x, ...) 
{
  	inv <- x$getInversedMatrix ()
  	if(!is.null(inv))
  	{
  	  	message("No Need To Calculate!")
  	  	return(inv)
 	 }
 	 data <- x$get()
 	 inv <- solve(data)
 	 x$setInversedMatrix (inv)
 	 inv
}
