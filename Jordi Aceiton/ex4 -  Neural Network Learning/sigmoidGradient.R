sigmoidGradient <- function(z) {
        
## =============================================================================
##
##      sigmoidGradient(z)      Compute the gradient of the sigmoid function 
##                              evaluated at each value of z (z can be a matrix,
##                              vector or scalar).        
##        
## =============================================================================                        
        
        # sigmoid function
        sgm <- function(z) {1/(1+exp(-z))}
        
        # sigmoid gradient function
        sgm_gradient <- sgm(z)*(1-sgm(z))        
}