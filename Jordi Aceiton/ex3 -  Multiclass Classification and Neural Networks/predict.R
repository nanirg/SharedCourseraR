predict <- function(Theta1, Theta2, X) {
        
## =============================================================================
##
##      predict(Theta1, Theta2, X)
##        
##      outputs the predicted label of X given the trained weights of a neural 
##      network (Theta1, Theta2) -- 3 layer network (input Lay., hidden L. and 
##      output Lay.)
##        
## =============================================================================        

        # Useful values
        m <- nrow(X)
        num_labels <- nrow(Theta2)     
        
        # Add x0 = 1 column to X training set.
        X <- cbind(rep(1,nrow(X)), X)
        
        # Define sigmoid function
        sigmoid <- function(z) {1/(1+exp(-z))}
        
        ## The feedforward propagation in neural network.
        # Notation according to Figure 2. The hidden layer
        z2 <- X %*% t(Theta1)
        a2 <- sigmoid(z2)
        
        a2 <- cbind(rep(1, nrow(X)), a2)   # add a20 = 1 bias column
        
        # Notation according to Figure 2. The output layer
        z3 <- a2 %*% t(Theta2)
        h <- sigmoid(z3)
        
        ## Prediction
        p <- apply(h, MARGIN = 1, FUN = which.max)
        
}