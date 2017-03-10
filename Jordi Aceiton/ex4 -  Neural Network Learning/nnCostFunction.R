nnCostFunction <- function(input_layer_size, hidden_layer_size, num_labels, 
                           X, y, lambda) 
{

## =============================================================================
##
##      nnCostFunction(input_layer_size, hidden_layer_size, num_labels, 
##                     X, y, lambda)(nn_params)
##
##      Implements the neural network cost function for a two layer
##      neural network which performs classification. Computes the cost of the 
##      neural network. The parameters for the neural network are "unrolled" into 
##      the vector nn_params and need to be converted back into the weight 
##      matrices.
##        
##      NOTE:  The nnCostFunction has its own arguments and another one (nn_params)
##             which is called for an inside function. This is a good trick to for(
##             allow R to pass functions as argumens of other functions. 
##             You can see how important it is in "checkNNGradients.R" and 
##             "ComputeNumericalGradient.R" function
##               
##        
##      input:
##
##              input_layer_size -- number of neurons in the input layer. It matches
##                                  the number of columns in Theta1 - 1 column .
##        
##              hidden_layer_size -- number of neurons in the hidden layer. It matches
##                                   the number of rows in Theta1, and 
##                                   columns in Theta2 - 1 column.                
##
##              num_labels -- number of neurons in the output layer. It matches
##                            the number of rows in Theta2.        
##        
##              X -- mxn matrix of m examples of n features (input)
##
##              y -- m vector with the classification labels.
##        
##              lambda -- Regularization parameter.
##        
##        
##        
##      output: The J cost function for the 2 layered neural network
##        
## =============================================================================

        ## Function inside function to allow pass nnCostFunction as an argument.
        ## All code for nnCostFunction lies inside its one.
        
        function(nn_params) {
                # Reshape nn_params back into the parameters Theta1 and Theta2, 
                # the weight matrices for our 2 layer neural network
                
                # Matrix Theta1
                elements_Theta1 <- nn_params[1:(hidden_layer_size * (input_layer_size + 1))]
                rows_Theta1 <- hidden_layer_size
                cols_Theta1 <- input_layer_size + 1       
                
                Theta1 <- matrix(elements_Theta1, nrow= rows_Theta1, ncol= cols_Theta1 )
                
                # Matrix Theta2
                elements_Theta2 <- 
                    nn_params[(1+(hidden_layer_size*(input_layer_size + 1))):length(nn_params)]
                rows_Theta2 <-  num_labels
                cols_Theta2 <- hidden_layer_size + 1 
                
                Theta2 <- matrix(elements_Theta2, nrow= rows_Theta2, ncol= cols_Theta2 )
                
                # Setup some useful variables
                m = nrow(X)  # number of examples.

                ## Part 1: Feedforward the neural network and return the cost in the 
                ## variable J.
        
                # Add x0 = 1 feature to all examples.
                X <- cbind(rep(1, m), X)  
        
                # Define Sigmoid function
                sigmoid <- function(z) {1/(1+exp(-z))}
        
                # Feedforward algorithm
                z2 <- X %*% t(Theta1)
                a2 <- sigmoid(z2)
                a2 <- cbind(rep(1, m), a2) # add bias column a20
        
                z3 <- a2 %*% t(Theta2)
                h <- sigmoid(z3)
        
                # Convert the y values (1, 2, 3, 4, ..., K) to y K dim - vectors
                # of 1 and 0. For example: y = 1  >>  y = (1,0,0, ... ,0)
                y_vects <- matrix(0, nrow= m, ncol= num_labels)
        
                for (i in 1:m) {
                        y_vects[i, y[i]] <- 1
                }
        
                ## Compute J cost function for neural network J = J0 + JregTerm
                # h is a m x K matrix
                # y_vects is a m x K matrix
                # the operations inside sums are element-wise for each i,k
        
                # J without regularization.
                J0 <- 1/m * sum(colSums(-y_vects*log(h)-(1-y_vects)*log(1-h)))
        
                # Regularization term
                sum1 <- sum(colSums(Theta1[,2:ncol(Theta1)]^2))
                sum2 <- sum(colSums(Theta2[,2:ncol(Theta2)]^2))       
                JregTerm <- lambda/(2*m) * (sum1 + sum2)
                
                # J with regularization
                J <- J0 +JregTerm     
        }
}