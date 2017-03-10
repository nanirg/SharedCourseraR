randInitializeWeights <- function(L_in, L_out) {

## =============================================================================
##
##      randInitializeWeights(L_in, L_out) 
##        
##      randomly initializes the weights of a layer with L_in incoming 
##      connections and L_out outgoing connections.    
##      
##      input:
##        
##              L_in -- Incoming layer connections
##                
##              L_out -- Outcoming layer connections
##                
##      output: 
##               
##              randTheta_init -- (L_in + 1) x L_out matrix with the theta_ij
##                                randomly initialized between [-epsilon, epsilon]. 
##                                The L_in + 1 counts for the bias input.
##                
## =============================================================================        
 
        # The random values are selected in rang [-epsilon, epsilon] by the
        # algorithm   2 * epsilon * rand(0,1)  - epsilon. rand(0,1) are  random
        # elements uniformly distributed on the interval (0, 1)
        epsilon <- 0.12
        dim <- (L_in+1) * L_out 
        
        r <- 2*epsilon*runif(dim, 0, 1) - epsilon
        
        randTheta_init <- matrix(r, nrow= L_out, ncol= L_in+1)             
}