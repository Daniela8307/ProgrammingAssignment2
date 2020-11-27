## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##Se comparten los argumentos de la función que realizará la matriz
makeCacheMatrix <- function(x = matrix()) {
        
        # todo error if x is not a matrix
        inv <- NULL 
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



# Write a short comment describing this function
##Es muy similar a la función makeVector, solo que en lugar de la media se cambia por la inversa
##La misma asignación comenta que la función que devuelve la inversa de la matriz es la función solve ()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data) #recibe la matriz y devuelve su inversa de acuerdo al hint
        x$setinverse(inv)
        inv
}
#Se realizaron un par de ejercicios para confirmar que en efecto devuelve la inversa de la matriz.
        
