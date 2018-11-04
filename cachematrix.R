
## File name cacheMatrix.R
## Created on 11/4/2018
## Author Manoj Medarametla

## This function creates a special "matrix" object that can cache its inverse
## I added some basi validations for input parameters

makeCacheMatrix <- function(x = matrix()) {
        
        if(class(x) != "matrix"){
                message(x," is not of type matrix")
                return()
        }
        
        #Declaring a variable to hold Inverse matrix
        inv <- NULL
        
        #set method of the class
        set <- function(y){
                if(class(y) != "matrix"){
                        message(y, " is not of type matrix")
                        return()
                }
                x<<-y
                inv<<-NULL
        }
        
        #get method of the calss
        get <- function() x
        
        #allows to manually set the inverse value(Should be matrix!)
        setInverse <- function(val) {
                if(class(val) != "matrix"){
                        message(val, " is not of type matrix")
                        return()
                }
                inv<<-val
        }
        
        #get the inverse value
        getInverse <- function() inv
        
        list(set = set
        , get=get
        , setInverse = setInverse
        , getInverse = getInverse
        )
        
}


# cacheSolve: This function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then the cachesolve should retrieve the inverse from 
# the cache

# recalc calculates the inverse though value exists in the cache
# this is a door to recalculate if a wrong inverse is set using setInverse method 
# of makeCacheMatrix 

# Pleae note signature is deviation from what is mentioned int he original file
# however I tool lenience because recalc has a default value and the original 
# signature works as is

cacheSolve <- function(x,recalc=FALSE, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        if(!class(x)!="makeCacheMatrix"){
                message("x is not of type makeCacheMatrix")
                return()
        }
        
        inv <- x$getInverse()
        if(!is.null(inv) & !recalc){
                message("Getting cached data")
                return(inv)
        }
        
        inv <- solve(x$get(),...)
        x$setInverse(inv)
        inv
        
}
