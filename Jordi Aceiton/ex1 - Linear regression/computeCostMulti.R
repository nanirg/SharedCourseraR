computeCostMulti <- function(X, y, theta) {

## =============================================================================                  
##
##   computeCostMulti(X, y, theta) computes the cost of using theta as the
##   parameter for linear regression to fit the data points in X and y
##
## =============================================================================  
                
        # Initialize some useful values
        m = length(y);  # number of training examples 
        h = X %*% theta
        
        J = 1/(2*m) * sum((h-y)^2)
}