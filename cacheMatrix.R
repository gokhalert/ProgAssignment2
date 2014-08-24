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
