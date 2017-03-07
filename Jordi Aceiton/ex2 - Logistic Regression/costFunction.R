costFunction <- function(theta, X, y) {
        
## =============================================================================        
##   costFunction(theta, X, y) computes the cost of using theta as the
##   parameter for logistic regression and the gradient of the cost w.r.t. to                 
##   the parameters.
##
##   input:     X  a matrix containing {x0=1, x1, x2, ..., xn} features.
##              y  a vector containing {y} values
##              theta  the vector theta to compute J(theta) and grad(theta).        
##
##   output:    a list which elements are: J and grad
##
##              "J"         The J(theta) value computed 
##                         
##              "grad"      The grad(theta) vector computed        
##                     
## =============================================================================         

## =============================================================================
## It also contains JcostFunction and gradient to compute same as costFunction 
## but needed to use optim R function (the equivalent fminunc OCTAVE function)
## =============================================================================
        
        # Initialize some useful values
        m <- length(y)        
        
        # Sigmoid function
        sigmoid <- function(z) { 1/(1+exp(-z)) }
        
        # Variables needed to compute J and grad.
        z <- X %*% theta
        h <- sigmoid(z)
        
        # Cost functon J(theta)
        j <- 1/m * sum( (-y*log(h)) - (1-y)*log(1-h) )
        
        # Gradient function
        grad <- numeric()
        for (i in 1:ncol(X)) {
                grad[i] <- 1/m * sum( (h-y) * X[ ,i] )
        }
        # Return values
        return_values <- list(J= j, Gradient= grad)
                
}

## optim function needs cost function and gradient function to be computed
## in separated functions:

JcostFunction <- function(theta, X, y) {
        
        # Initialize some useful values
        m <- length(y)        
        
        # Sigmoid function
        sigmoid <- function(z) { 1/(1+exp(-z)) }
        
        # Variables needed to compute J and grad.
        z <- X %*% theta
        h <- sigmoid(z)
        
        # Cost functon J(theta)
        j <- 1/m * sum( (-y*log(h)) - (1-y)*log(1-h) )   
        j
} 

gradient <- function(theta,X,y) {
        # Initialize some useful values
        m <- length(y)        
       
        # Sigmoid function
        sigmoid <- function(z) { 1/(1+exp(-z)) }
        
        # Variables needed to compute J and grad.
        z <- X %*% theta
        h <- sigmoid(z)
        
        # Gradient function
        grad <- numeric()
        for (i in 1:ncol(X)) {
                grad[i] <- 1/m * sum( (h-y) * X[ ,i] )
        }
        grad 
}
