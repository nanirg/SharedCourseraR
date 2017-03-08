predictOneVsAll <- function(all_theta, X) {

## =============================================================================  
##
##      predictOneVsAll(all_theta, X)
##        
##      Predicts the label for a trained one-vs-all classifier. The labels 
##      are in the range 1, 2, ...,K. It returns a vector of predictions
##      for each example in the matrix X.
##
##      input:
##        
##              all_theta -- is a matrix where the i-th row is a trained logistic
##                           regression theta vector for the i-th class. 
##
##              X -- training set of n features and m examples.        
##        
##      output:
##
##              pred -- Vector of values from 1 to K predicting the examples class.       
##   
## =============================================================================               
        
        ## Add x0 = 1 column to X matrix
        X <- cbind(rep(1, nrow(X)), X)
        
        ## Compute the z(i,j) for each theta(j) j = 1, ... , k. i = 1, ... m 
        ## k is set by all_theta number of rows.
        z <- X %*% t(all_theta)
        
        ## compute h(i,j) for each each theta(j) j = 1, ... , k. i = 1, ... m
        sigmoid <- function(z) {1/(1+exp(-z))}
        h <- sigmoid(z)
        
        ## To predict to which class each example belongs, we look for the max 
        ## h(i-fixed, j=1,...,k) value and the column number is the class  (j)
        
        pred <- apply(h, MARGIN = 1, FUN = which.max)
        
}