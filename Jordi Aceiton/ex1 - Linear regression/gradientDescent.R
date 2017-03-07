gradientDescent <- function(X, y, theta, alpha, num_iters) {
        
##  GRADIENTDESCENT Performs gradient descent to learn theta.
##
##                  Returns a list of 2 components:
##
##                    1) Trained theta. (the one that minimizes J cost function)
##                    2) The J values obtained through the gradient descent steps.

        # number of training examples.
        m <- length(y)
        
        # gradient descent algorithm
        jhistory <- numeric()
        
        for (i in 1:num_iters) {
               
                h <- X %*% theta 
                
                theta[1] <- theta[1] - alpha/m * sum((h - y)*X[,1])
                theta[2] <- theta[2] - alpha/m * sum((h - y)*X[,2])
                
                jhistory[i] <- computeCost(X, y, theta)  
        } 
        
        
        # Double variable return for the function using a list.
        return_values <- list( Theta= theta , Jhistory= jhistory)
}