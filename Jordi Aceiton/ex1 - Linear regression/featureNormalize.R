featureNormalize <- function(x) {

## =============================================================================        
##   featureNormalize(x) returns a normalized version of X where
##   the mean value of each feature is 0 and the standard deviation
##   is 1. This is often a good preprocessing step to do when
##   working with learning algorithms.                
        
##   input:     X  a dataframe containing {x1, x2, ..., xn} features.
##   output:    a list which elements are: X_norm, mu, sigma
##
##              "X_norm"    a dataframe containing {u1, u2, ... ,un} normalized 
##                          features.
##              "mu"        the mean values for each feature {x1, x2, ...}        
##              "sigma"     the sd value for each feature {x1, x2, ...}        
## =============================================================================        
     
           
        # Means of the features of X. 
        mu <- colMeans(X)
        
        # standard deviations of the features of X.
        sigma <- apply(X, MARGIN = 2, FUN = sd)
        
        # Feature normalization. apply -> t (transpose) -> as.data.frame
        X_norm <- as.data.frame(t(apply(X, MARGIN = 1, function(x) (x-mu)/sigma)))
        
        # X_norm[1] = Nan because of sigma(1) = 0 being the divisor.
        X_norm[1] = rep(1, nrow(X_norm))
        
        # Return X_norm, mu, sigma.
        return_values <- list( X_norm= X_norm, Mu= mu, Sigma= sigma )        
}