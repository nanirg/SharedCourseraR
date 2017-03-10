debugInitializeWeights <- function(fan_out, fan_in) {
        
## =============================================================================
##
##      debugInitializeWeights(fan_out, fan_in)
##        
##              initializes the weights of a layer with fan_in incoming 
##              connections and fan_out outgoing connections using a fix set of 
##              values.
##              Note that W should be set to a matrix of size 
##              (fan_out)X(1 + fan_in) as   the first row of W handles the     
##              "bias" terms. 
##        
##      input:
##
##              fan_out -- outgoing connections
##
##              fan_in -- incoming connections
##        
##      output:
##
##              W -- (fan_out)X(1 + fan_in) Matrix of initialized Weights        
##        
## =============================================================================          
        
        
        # Set W to zeros
        W <- matrix(0, nrow= fan_out, ncol= 1+fan_in)
        
        # Initialize W using "sin", this ensures that W is always of the same
        # values and will be useful for debugging
        
        elements <- sin(1:length(W))
        W <- matrix(elements, nrow= nrow(W), ncol= ncol(W)) / 10
        
}