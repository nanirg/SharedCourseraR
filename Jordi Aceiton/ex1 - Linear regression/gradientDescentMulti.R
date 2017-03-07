gradientDescentMulti <- function(X, y, theta, alpha, num_iters) {
        
## =============================================================================
## gradientDescentMulti(X, y, theta, alpha, num_iters) Performs gradient descent
## to learn theta.
##
## Returns a list of 2 components:
##
##    1) Theta: Trained theta. (the one that minimizes J cost function)
##    2) Jhistory: The J values obtained through the gradient descent steps.
##
## Usage: 
##
##        theta = GRADIENTDESCENTMULTI(x, y, theta, alpha, num_iters)[["Theta"]] 
##        updates theta by taking num_iters gradient steps with learning rate 
##        alpha.        
## =============================================================================        

        source("computeCostMulti.R")
        
        # number of training examples.
        m <- length(y)
        
        # gradient descent algorithm
        X <- as.matrix(X)
        jhistory <- numeric()
        thetahistory <- numeric()
        
        for (i in 1:num_iters) {
                
                h <- X %*% theta 
                
                for (j in 1:length(theta)) {
                        theta[j] <- theta[j] - alpha/m * sum((h - y)*X[,j])
                }
                jhistory[i] <- computeCostMulti(X, y, theta) 
        } 
        
        return_values <- list(Theta = theta, Jhistory = jhistory)       
}