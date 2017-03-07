costFunctionReg <- function(theta, X, y, lambda) {
        
## =============================================================================        
##   costFunctionReg(theta, X, y, lambda)   
##
##      Compute cost and gradient for logistic regression with regularization
##
##   input:     X  a matrix containing {x0=1, x1, x2, ..., xn} features.
##              y  a vector containing {y} values
##              theta  the vector theta to compute J(theta) and grad(theta).  
##              lambda   the regularization parameter.
##
##   output:    a list which elements are: J and grad
##
##              "J"         The J(theta) value computed 
##                         
##              "grad"      The grad(theta) vector computed        
##                     
## =============================================================================            
## =============================================================================
## It also contains JcostFunctionReg and gradientReg to compute same as  
## costFunctionReg but needed to use optim R function (the equivalent fminunc 
## OCTAVE function)
## =============================================================================

        # Initialize some useful values
        m <- length(y)        # number of examples
        n <- length(theta)    # number of features + 1.
        
        # Sigmoid function
        sigmoid <- function(z) { 1/(1+exp(-z)) }
        
        # Variables needed to compute J and grad.
        z <- X %*% theta    # z(i) = theta0*x0(i) + theta1*x1(i) + ...
        h <- sigmoid(z)     # h(i) = h(X(i)) 
        
        # Cost functon J(theta) = j0 + regTerm
        j0 <- 1/m * sum( (-y*log(h)) - (1-y)*log(1-h) )
        jRegTerm <- lambda/(2*m) * sum(theta[2:n]^2)
        
        j <- j0 + jRegTerm
        
        # Gradient function grad = grad0 + gradRegTerm
        grad0 <- numeric()
        for (i in 1:n) {
                grad0[i] <- 1/m * sum( (h-y) * X[ ,i] )
        }
        
        gradRegTerm <- lambda/m * c(0, theta[2:n])   # gradRegTerm[1] = 0
                
        grad <- grad0 + gradRegTerm
        
        # Return values
        return_values <- list(J= j, Gradient= grad)
}

JcostFunctionReg <- function(theta, X, y,lambda) {
        
        # Initialize some useful values
        m <- length(y)        # number of examples
        n <- length(theta)    # number of features + 1.
        
        # Sigmoid function
        sigmoid <- function(z) { 1/(1+exp(-z)) }
        
        # Variables needed to compute J and grad.
        z <- X %*% theta
        h <- sigmoid(z)
        
        # Cost functon J(theta) = j0 + regTerm
        j0 <- 1/m * sum( (-y*log(h)) - (1-y)*log(1-h) )
        jRegTerm <- lambda/(2*m) * sum(theta[2:n]^2)
        
        j <- j0 + jRegTerm   
} 

gradientReg <- function(theta,X,y,lambda) {

        # Initialize some useful values
        m <- length(y)        # number of examples
        n <- length(theta)    # number of features + 1.        
        
        # Sigmoid function
        sigmoid <- function(z) { 1/(1+exp(-z)) }
        
        # Variables needed to compute J and grad.
        z <- X %*% theta
        h <- sigmoid(z)
        
        # Gradient function grad = grad0 + gradRegTerm
        grad0 <- numeric()
        for (i in 1:n) {
                grad0[i] <- 1/m * sum( (h-y) * X[ ,i] )
        }
        
        gradRegTerm <- lambda/m * c(0, theta[2:n])   # gradRegTerm[1] = 0
        
        grad <- grad0 + gradRegTerm
}
