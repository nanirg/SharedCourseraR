checkNNGradients <- function(lambda= 0) {
        
## =============================================================================
##
##      checkNNGradients(lambda= 0) 
##        
##            Creates a small neural network to check the backpropagation 
##        gradients, it will output the analytical gradients produced by your 
##        backprop code and the numerical gradients (computed using 
##        computeNumericalGradient). These two gradient computations should                                                   
##        result in very similar values. 
##
##      input:  lambda -- Regularizaton term. Initialized to 0 value.
##        
##        
## =============================================================================        

        # Neural network parameters
        input_layer_size <- 3
        hidden_layer_size <- 5
        num_labels <- 3
        m <- 5
        
        # Generate some 'random' test data
        Theta1 <- debugInitializeWeights(hidden_layer_size, input_layer_size)
        Theta2 <- debugInitializeWeights(num_labels, hidden_layer_size)
        
        # Reusing debugInitializeWeights to generate X
        X  <- debugInitializeWeights(m, input_layer_size - 1)
        
        y  <- 1 + 1:m %% num_labels

        # Unroll parameters
        nn_params <- c(Theta1,Theta2)
        
        # Short hand for cost function
        J_CostFunc <- nnCostFunction(input_layer_size, hidden_layer_size, num_labels,
                                     X, y, lambda) # no (nn_params passed!!!)
        
        cost <- J_CostFunc(nn_params)

        grad <- nnGradFunction(input_layer_size, hidden_layer_size, num_labels, 
                                    X, y, lambda)(nn_params)
        
        numgrad <- computeNumericalGradient(J_CostFunc, nn_params)

        # Visually examine the two gradient computations.  The two columns
        # you get should be very similar.
        
        showMatrix <- matrix(c(numgrad,grad), nrow = length(grad), ncol = 2)
        
        print(showMatrix)
        
        cat("\nThe above two columns you get should be very similar.\n",
            "(Left-Your Numerical Gradient, Right-Analytical Gradient)\n")
        

}


