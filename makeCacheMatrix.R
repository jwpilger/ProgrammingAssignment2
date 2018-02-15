# Coursera Programming Assignment 2
# John Pilger
# test_assigment2 - test multiple use cases of the two functions
#
test_assign2 = function(mat_rowcol){  # test the two functions and check the times
   
  
    message("starting matrix creation for inversion calculation ")
    
    mat1 <- matrix(rnorm(mat_rowcol^2),nrow=mat_rowcol,ncol=mat_rowcol)  # create square matrix of random# with dim parm
    orig_list <- makeCacheMatrix(mat1) 
    
    message(c("Matrix created - cache status ",orig_list[2]))
    message(c("Original Matrix dimensions row/column ",dim(orig_list[[1]])))
    
    
    inv_list <-  cacheSolve(orig_list[[1]])
    message(c(round(dur_1 <- inv_list[[3]],digits=6)," seconds elasped - first  call "))
    
    inv_list <-  cacheSolve(orig_list[[1]])
    message(c(round(dur_2 <- inv_list[[3]],digits=6)," seconds elasped - second call "))
    
    message(c(round(dur_1-dur_2,digits=6)," seconds delta with caching enabled"))

    message(paste(round((dur_1-dur_2)/dur_1*100,digits=2)," % elasped time savings with caching enabled"))
    message(c("Inverted Matrix dimensions row/column ",dim(inv_list[[1]])))
    
    inv_list <-  cacheSolve(orig_list[[1]])
    message(c(round(dur_2 <- inv_list[[3]],digits=6)," seconds elasped - third   call "))
    
    assign("orig_matrix_global",incorrect_m <- matrix(),envir=globalenv())     # set incorrect matrix 
    inv_list <-  cacheSolve(orig_list[[1]])
    message(c(round(dur_2 <- inv_list[[3]],digits=6)," seconds elasped - forth   call "))
    message(c("Original Matrix dimensions row/column ",dim(incorrect_m)))
    
    #rm("inv_matrix_global","inv_status_global",orig_matrix_global,envir=globalenv()) # remove global variables
    #ls(globalenv())
}

makeCacheMatrix <- function(mat1 = matrix()) {
 # makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
 # For this assignment, assume that the matrix supplied is always invertible.
    
  
  
    inv_status <- assign("inv_status_global",FALSE,envir=globalenv())   # set cache status to FALSE (need to calc)
    assign("orig_matrix_global",mat1,envir=globalenv())                 # original matrix

    return(list(mat1,inv_status))                               # return list pointing to matrix, status in global env
}

cacheSolve <- function(mat_solve, ...) {
# 	cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
#   If the inverse has already been calculated (and the matrix has not changed),
#    then the cachesolve should retrieve the inverse from the cache
    
    dur_start <- Sys.time()             # record the start time
 
    y<- get("orig_matrix_global",envir=globalenv())     # current matrix used to calc inversion
    same_matrix <- matequal(mat_solve,y)                # is this the same matrix as original calc for inverse?
    
    inv_check <- get("inv_status_global",envir=globalenv())     # get global matrix inverse status true=inverse exists
   
    if (inv_check == TRUE && same_matrix == TRUE){                 # has the matrix inverse been solved? Same matrix?
        message("retrieving inverse matrix cached copy")        # don't recalc
        dur_1 <- as.numeric(difftime(Sys.time(), dur_start, units="secs"))  # calc function duration
        return(list(inv_matrix_global,inv_status_global,dur_1,same_matrix)) # return list pointing to existing inv matrix
    }
    
  
    message("calculating matrix inversion - storing in global env")     
    assign("orig_matrix_global",mat_solve,envir=globalenv())            # store original matrix in global env
    assign("inv_matrix_global",solve(mat_solve, ...),envir=globalenv()) # calc inverted matrix; put in global env
    assign("inv_status_global",TRUE,envir=globalenv())                  # update cache status - true = exists; don't recalc
   
   
    
    dur_1 <- as.numeric(difftime(Sys.time(), dur_start, units="secs")) # calc function duration (secs) numeric
    return(list(inv_matrix_global,inv_status_global,dur_1,same_matrix)) # return list of inverted matrix, cache status, function duration(secs)
}

# Check if two matrices have identical values (disregarding dimnames et al.)
matequal <- function(x, y) {
    is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
}
