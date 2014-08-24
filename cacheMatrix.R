#makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
	   inverseMat <- NULL
	   set <- function(y) {
	   			x <<- y
	   			inverseMat <<- NULL
	   		}
	   		get <- function() x
	   		setInverseMat <- function (solve) inverseMat <<- solve
	   		getInverseMat <- function() inverseMat
	   		list(set = set, get = get, setInverseMat = setInverseMat,
	   		getInverseMat = getInverseMat)
	}

#cacheSolve	

cacheSolve <- function(x,...) {
		inverseMat <- x$getInverseMat()
		if(!is.null(inverseMat)) {
				message("getting cached data")
				return(inverseMat)
			}
			data <- x$get()
			inverseMat <- solve(data,...)
			x$setInverseMat(inverseMat)
			inverseMat
		}
		
#solution

> x <- matrix(1:4,nrow =2 , ncol = 2)
> ex1 <- makeCacheMatrix(x)
> cacheSolve(ex1)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> cacheSolve(ex1)
getting cached data
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

> try <- cacheSolve(ex1)
getting cached data
> 
> x %*% try
     [,1] [,2]
[1,]    1    0
[2,]    0    1
> 

