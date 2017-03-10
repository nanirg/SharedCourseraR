computeNumericalGradient <- function(J, theta) {
        
## =============================================================================
##
##      computeNumericalGradient(X, y, Theta1, Theta2, lambda) 
##        
##              computes the numerical gradient of the function J around theta 
##              using "finite differences" and gives us a numerical estimate. 
##              To do it calls "nnCostFunction.R" file that must be inside the
##              working directory.        
##
##      input:  
##        
##              J -- The cost function definition to be calculated with no
##                   (nn_params) passed.
##        
##                   i.e: nnCostFunction(input_layer_size, hidden_layer_size, 
##                                         num_labels, X, y, lambda)(nn_params)        
##
##              theta -- the nn_params value needed to finally evaluate J.
##                
##      output:
##                
##              numgrad -- numeric gradient 1 and numeric gradient 2 all them 
##                         unrolled into a vector to be rolled later.         
##        
##        
## =============================================================================         
        

        # Return values
#        return_val <- list(NumGrad1 = numgrad1, NumGrad2= numgrad2)

        numgrad <- rep(0,length(theta))
        perturb <- rep(0,length(theta))
        e <- 1e-4
        for (p in 1:length(theta)) {
                # Set perturbation vector
                perturb[p] <- e
                loss1 <- J(theta - perturb)
                loss2 <- J(theta + perturb)
                # Compute Numerical Gradient
                numgrad[p] <- (loss2 - loss1) / (2 * e)
                perturb[p] <- 0
        }
        
        numgrad
}        
   


