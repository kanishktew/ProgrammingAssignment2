## The functions below use the lexical scoping (static scoping) feature of R to
## optimize computing the inverse of a matrix, by caching the same, so that if
## asked for the inverse of the same matrix, it is not recomputed, and the
## required matrix is already ready. A special kind of assignment operator,
## '<<-' is also used for changing values of objects in multiple environments,
## which helps in caching.


## The 'makeCacheMatrix' creates a 'special matrix' - actually a list - which
## can be accessed and modified through various functions defined in its
## environment.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        
        get<-function() x
        
        setinverse<-function(inv) m<<-inv
        # The '<<-' assignment operator changes value of 'm' variable
        # even in the defining environment of the function, hence making
        # it possible to cache the inverse value
        
        getinverse<-function() m
        
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
        # the list-'special matrix'-contains functions to control the matrix
}


## The 'cacheSolve' function takes the square invertible 'special matrix', and
## returns its inverse, which is again cached in the same matrix. If the matrix is
## unchanged, then this function, instead of recomputing the inverse, just returns
## the cached value in the matrix. This is possible through the lexical scoping
## property of R

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<-x$getinverse()
        
        if(!is.null(i)){ # to make sure that inverse is computed only once
                retun(i)
        }
        
        data<-x$get()
        if(nrow(data)==ncol(data)){ # this checks if matrix is a square matrix
                i<-solve(data, ...)
        }
        else{
                stop("Not a square matrix!!!")
        }
        
        x$setinverse(i)
        i
}
