nnGradFunction <- function(input_layer_size, hidden_layer_size, num_labels, 
                        X, y, lambda)   {

## =============================================================================
##
##      nnGradients(X, y, Theta1, Theta2, lambda)
##        
##              Implements the neural network Gradients for a two layer
##              neural network which performs classification. Uses the
##              Backpropagation algorithm.     
##        
##      NOTE:  The nnGradients has its own arguments plus another one (nn_params)
##             which is called for an inside function. This is a good trick to for(
##             allow R to pass functions as argumens of other functions.                
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
##      output: The gradients for the neural network but unrolled
##        
## =============================================================================        

        
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
                n = ncol(X)  # number of features x1, x2, x3, ..., xn.
          
        
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
        
        
## Part 2: The compute of gradients by Backpropagation:

               # Acumulator initialization
                DELTA_2 <- matrix(0, nrow= nrow(Theta2), ncol= ncol(Theta2))  
                DELTA_1 <- matrix(0, nrow= nrow(Theta1), ncol= ncol(Theta1)) 
        
                # Backpropagation algorithm. For each m examples in X
                for (i in 1:m) {
                
                        # K dimensional "error term" vector for the i-th example
                        delta3 <- h[i, ] - y_vects[i, ]
                
                        delta2 <- t(Theta2) %*% delta3 * a2[i, ]*(1-a2[i, ])
                        delta2 <- delta2[2:nrow(delta2), ]    #remove delta2_0
                
                        DELTA_2 <- DELTA_2 + delta3 %o% a2[i, ]
                        DELTA_1 <- DELTA_1 + delta2 %o% X[i, ]
                }
        
                # Compute gradients
                Theta1_grad_0 <- 1/m * DELTA_1
                Theta2_grad_0 <- 1/m * DELTA_2
        
                Theta1_grad_reg <- lambda/m * Theta1
                Theta2_grad_reg <- lambda/m * Theta2
        
                # To consider both cases j = 0 i j!=0, we declare first all values and 
                # then substract the regularizatin term for j = 0 .
                Theta1_grad <- Theta1_grad_0 + Theta1_grad_reg
                Theta2_grad <- Theta2_grad_0 + Theta2_grad_reg
        
                Theta1_grad[ ,1] <- Theta1_grad[ ,1] - Theta1_grad_reg[ ,1]   # Case j=0 
                Theta2_grad[ ,1] <- Theta2_grad[ ,1] - Theta2_grad_reg[ ,1]   # Case j=0

                # return values
                grads <- c(Theta1_grad, Theta2_grad)

        }
}        