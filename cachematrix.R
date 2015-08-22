# set the value of the vector
# get the value of the vector
# set the value of the mean
# get the value of the mean


## makeCacheMatrix uses scoping to store matrices in memory with the: <<- 

makeCacheMatrix <- function(YuM = matrix()) {
        inverse <- NULL
        set <- function(Tebow){
                YuM <<- Tebow
                inverse <<- NULL
        }
        get <- function() YuM
        setinverse <- function(Inverse) inverse <<- Inverse
        getinverse <- function() inverse
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## Write a short comment describing this function
## cacheSolve uses corpcor, to do some fantastic storing
## note: this function will try to load corpcor library. If it's not installed will try to install the library

cacheSolve <- function(YuM, ...) 
{
        if(require("corpcor")){
                print("corpcor is loaded correctly")
        } else {
                print("trying to install corpcor")
                install.packages("corpcor")
                if(require(corpcor)){
                        print("corpcor installed and loaded")
                } else {
                        stop("could not install corpcor")
                }
        }
        inverse <- YuM$getinverse()
        if(!is.null(inverse)){
                message("matrix is in memory")
                return(inverse)
        }
        message("inverse is not in memory so the inverse (if exist) is gonna be computed")
        data <- YuM$get()
        inverse <- pseudoinverse(data, ...)
        YuM$setinverse(inverse)
        inverse
}

## Please, please,please,please
