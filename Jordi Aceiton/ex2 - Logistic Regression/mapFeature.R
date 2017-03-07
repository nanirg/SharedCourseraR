mapFeature <- function(X1, X2) {
        
## =============================================================================        
##   mapFeature(X1, X2)         Returns a new feature array with more features,  
##                              comprising of X1, X2, X1.^2, X2.^2, X1*X2, 
##                              X1*X2.^2, etc..
##                              Inputs X1, X2 must be the same size
##
##   input:     two m-vector features X1 and X2.       
##
##   output:    a new X matrix with the new polynomial features.        
##                     
## =============================================================================           
        X <- matrix(1, nrow = length(X1), ncol=1)
        
        degree <- 6
        for (i in 1:degree) {
                for (j in 0:i) {
                       X <- cbind(X, X1^(i-j) * X2^j ) 
                }
        }
        X
}