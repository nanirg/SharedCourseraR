oneVsAll <- function(X, y, num_labels, lambda) {
        
## =============================================================================
##
##      oneVsAll(X, y, num_labels, lambda)
##        
##              trains "num_labels" logistic regression classifiers and returns 
##              each of these classifiers in a matrix all_theta, where the i-th  
##              row of all_theta corresponds to the classifier for label i
##        
##        input:
##                X -- n-feature training data set with m examples.
##        
##                y -- labels to X data
##                
##                num_labels -- the number of diferent labels to X data.
##                        
##                lambda -- regularization parameter.
##                
##        output:
##                all_theta -- all the classifiers in a matrix all_theta, where  
##                             the i-th row of all_theta corresponds to the
##                             classifier for label i.
##
## ==============================================================================
        
        # Some useful variables
        m <- nrow(X)
        n <- ncol(X)
        
        # Add ones to the X data matrix. The x0 feature
        X <- cbind( rep(1, m), X)        
        
        ## Optimize J function using optim R function for each y-label.
        initial_theta <- rep(0, n+1)
        all_theta <- matrix(0, nrow= num_labels, ncol= n+1)
        
        cat ("Training One-vs-All Logistic Regression...\n\n")
        
       for (k in 1:num_labels) {
              
                cat("For label: ", k, "Cost function: ...  ")
                
                optimRes <- optim(par= initial_theta,
                                  fn= lrCostFunction, 
                                  X= X, 
                                  y= as.numeric(y==k), 
                                  lambda= lambda,  # assign explicitly the argument values.
                                  gr= lrGradient, 
                                  method= "BFGS",
                                  control= list(maxit = 75))
                
                cat(optimRes$value,"\n")
                all_theta[k,] <- optimRes$par
        } 
        all_theta               
}