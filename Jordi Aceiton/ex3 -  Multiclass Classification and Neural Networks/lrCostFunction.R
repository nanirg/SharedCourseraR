lrCostFunction <- function(theta, X, y, lambda) {
        
##      lrCostFunction(theta, X, y, lambda) 
##        
##      Returns de J(theta, X, y, lambda) cost function with regularization for 
##      logistic regression. 
##
##      input:
##        
##              theta -- The parameter vector h = sigmoid(theta0*x0 + theta1*x1 + ...)
##        
##              X -- The n-features x0,x1,x2...xn that describes the m examples 
##                   x(i) in the training dataset. Notice that it handles x0 = 1
##                   feature.                
##        
##              y -- the values that labels the X data in logistic regression.
##                
##              lambda -- The regularization parameter.
##                
##      output:
##
##              J(theta, X, y, lambda)
##
## =============================================================================        
      
        ## Initialize some useful values
        m <- nrow(X)    # number of training examples   
        n <- ncol(X)    # number of features
                
        ## define sigmoid function
        sigmoid <- function(z) {1/(1+exp(-z))}
        
        ## Compute J function:
        
        # m-dimension vector z(i) = theta0*x0(i) + theta1*x1(i) + ...  
        z <- X %*% theta  
        
        # m-dimension vector h(i) = sigmoid( z(i) ) that is  h(x(i)).
        h <- sigmoid(z)
        
        # J = J0 + JregTerm
        J0 <- 1/m * sum((-y*log(h))-(1-y)*log(1-h))
        JregTerm <- lambda/(2*m) * sum(theta[2:n]^2)
        
        J <- J0 + JregTerm
}