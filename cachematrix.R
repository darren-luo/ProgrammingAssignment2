## makeCacheMatrix takes a matrix argument (m) and utilizes setting (setmatrix, setinverse) and
## getting (getmatrix, getinverse) functions to set and recall values of m and the required output 
## m_inverse. This is done by storing the value of m and m_inverse in the parent environment
## so that these values can be checked and called in the separate cacheSolve function below which references
## the set and get functions in makeCacheMatrix

makeCacheMatrix <- function(m = matrix()) {
        m_inverse <- NULL
        x <- matrix()
        setmatrix <- function(x) {
                m <<- x
                m_inverse <<- NULL
        }
        getmatrix <- function() {
                m
        }
        setinverse <- function(solve) {
                m_inverse <<- solve
        }
        getinverse <- function() {
                m_inverse
        }
        list (setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
}
cacheSolve <- function(m, ...) {
       m_inverse <- m$getinverse()
       if(!is.null(m_inverse)){
               message("getting cached data")
               return(m_inverse)
       }
       else {
               data <- m$getmatrix()
               m_inverse <- solve(data)
               m$setinverse(m_inverse)
               m_inverse
       }
}

