## makeCacheMatrix is a function which when called will return a special 
## version of matrix which is defined by four functions to set and get
## value of the matrix which is being proxied as well as get/set functions
## for the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	# initialize computed value of the inverse of the matrix NULL
	inverse_matrix <- NULL 
	# set matrix
	set <- function(m) { 
		x <<- m
		inverse_matrix <<- NULL
	}
	# return matrix
	get <- function() { 
		x
	}
	# set the computed value of the inverse of the matrix
	set_inverse <- function(in_inv) {
		inverse_matrix <<- in_inv
	}
	# return computed value of the inverse of the matrix
	get_inverse <- function() {
		inverse_matrix
	}
}


## cacheSolve is a function which computes the inverse of the
## special matrix which is created by calling makeCacheMatrix
## function above. Before calculating the inverse this function
## will invoke exposed method on the special matrix to check 
## whether inverse has already been computed. If the inverse 
## has been computed, it is returned. If the inverse has not 
## computed, it is computed and then cached
cacheSolve <- function(x, ...) {
	curr_inv <- x$get_inverse()
	if (!is.null(curr_inv)) {
		curr_inv
	} else {
		m <- x$get()
		curr_inv <- solve(m, ...)
		x$set_inverse(curr_inv)
		curr_inv
	}
}
