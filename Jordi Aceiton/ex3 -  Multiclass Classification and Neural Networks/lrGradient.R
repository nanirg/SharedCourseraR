lrGradient <- function(theta, X, y, lambda) {
        
##      lrGradient(theta, X, y, lambda) 
##        
##      Returns de partial(J(theta, X, y, lambda)) with respect to theta(j)  
##      with regularization for logistic regression. 
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
##              d(J(theta, X, y, lambda))/d(theta(j)) for j = 1, ..., n 
##
## =============================================================================        
        
        ## Initialize some useful values
        m <- nrow(X)    # number of training examples   
        n <- ncol(X)    # number of features
        
        ## define sigmoid function
        sigmoid <- function(z) {1/(1+exp(-z))}
        
        ## Compute gradient:
        
        # m-dimension vector z(i) = theta0*x0(i) + theta1*x1(i) + ...  
        z <- X %*% theta  
        
        # m-dimension vector h(i) = sigmoid( z(i) ) that is  h(x(i)).
        h <- sigmoid(z) 
        
        ## grad(j) = grad0(j) + gradRegTerm(j);  for j= 1, ..., n
        grad0 <- numeric()
        for (j in 1:n) {
                grad0[j] <- 1/m * sum( (h - y )*X[,j] )
        } 
        
        gradRegTerm <- c(0, lambda/m * theta[2:n])
        grad <- grad0 + gradRegTerm
}