normalEqn <- function(X, y) {
        
## =============================================================================  
##
##   normalEqn(X,y) computes the closed-form solution to linear 
##   regression using the normal equations.   
##        
##   input:     {X, y} training set.     
##
##   output:    theta value that optimizes the function cost.      
## =============================================================================          

        # Add the x0 = 1 examples.
        X <- as.matrix(X); colnames(X) <- NULL
                
        # The transpose matrix
        Xt <- t(X)
        
        # The normal Equation
        theta <- as.numeric(solve(Xt%*%X) %*% (Xt%*%y))
}