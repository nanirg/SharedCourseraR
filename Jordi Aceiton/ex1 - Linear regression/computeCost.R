computeCost <- function(X, y, theta) {

##  computeCost(X, y, theta):  Computes the cost J(theta) of using theta as 
##                             the parameter for linear regression to fit the 
##                             data points in X and y     
        

        # number of examples in the training set {X,y}
        m <- length(y)
        
        h <- X %*% theta  
                
        # Cost function for linear regression.
        J <- 1/(2*m) * sum((h-y)^2)
}