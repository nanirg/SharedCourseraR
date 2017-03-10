predict <- function(Theta1, Theta2, X) {
        
## =============================================================================
##
##      predict(Theta1, Theta2, X) 
##
##              outputs the predicted label of X given the trained weights of a 
##              neural network (Theta1, Theta2)
##
##      input:
##
##              X -- m examples of n featured training set.
##
##              Theta1 -- weights from the input layer
##
##              Theta2 -- weights from the hidden layer
##
##      ouput:
##
##              pred -- m vector of the Neural network labels prediction.
##        
## =============================================================================
        
        # Useful values
        m <- length(y) # number of examples.
        num_labels <- nrow(Theta2)
        
        # Add the x0 = 1 column to X
        X <- cbind(rep(1, length(y)), X) 
        
        # sigmoid function definition
        sigmoid <- function(z) {1/(1+exp(-z))}
        
        z2 <- X %*% t(Theta1)
        a2 <- sigmoid(z2)
        a2 <- cbind(rep(1, nrow(a2)), a2) 
        
        z3 <- a2 %*% t(Theta2)
        h <- sigmoid(z3)
        
        pred <- apply(h, MARGIN = 1, which.max)
}