## Machine Learning Online Class 
## Exercise 4 Neural Network Learning
## =============================================================================

## Initialization.
rm(list = ls())  # Clear workspace command.
cat("\014")      # Clear console.

## linking the needed functions.
source("displayData.R")
source("nnCostFunction.R")
source("nnGradFunction.R")
source("checkNNGradients.R")
source("sigmoidGradient.R")
source("randInitializeWeights.R")
source("debugInitializeWeights.R")
source("ComputeNumericalGradient.R")
source("lbfgsb3_.R")
source("predict.R")

## ==== Part 1: Loading and Visualizing Data ===================================
##
##      instructions:  Loading and visualizing the dataset that contains 
##                     handwritten digits. 
##
## =============================================================================

cat("***** EXERCISE 4 - PART 1: Loading and Visualizing Data *****\n\n") 
readline("Press ENTER to start...")

## load the data with R.matlab package to read .mat files
suppressMessages(suppressWarnings(
        
        if (require(R.matlab) == FALSE) {
                stop("\n\n Please, install R.matlab package To run this script")
        }  
))

## Package required to run optimization to train NN
suppressMessages(suppressWarnings(
        
        if (require(lbfgsb3) == FALSE) {
                stop("\n\n Please, nstall lbfgsb3 package To run this script")
        } 
))

cat("\nLoading data and plotting 100 random examples...")

## TRAINING SET:
#
# data contains training set {X,y}
#
#   X (mxn matrix): 
#       m = number of handwritten images 
#       n = 20x20 pixels with greyscale values: x1,... x400 features.
#   y (m vector) containing the labels (1,2,...,10) of the handwritten images. 
#
##
data <- readMat("ex4data1.mat")
X <- data$X
y <- data$y

## Randomly select 100 data points to display:
rand_indices <- sample(nrow(X))
rand_X_selection <- X[rand_indices[1:100], ]

## Call displayData() function sourced by displayData.R file.
displayData(rand_X_selection)

cat(" Ok!\n")

cat("\n")
readline("Press ENTER to continue...")
cat("\014")   # Clear console.
rm(data, rand_X_selection, rand_indices) # Cleaning workspace


## ==== Part 2: Loading Parameters ===============================================
##
##      instructions:   load some pre-initialized neural network parameters
##
## =============================================================================

cat("***** EXERCISE 4 - PART 2: Loading Parameters *****\n\n") 
readline("Press ENTER to start...")

cat("\nLoading Saved Neural Network Parameters ... ")

# Setup the parameters you will use for this exercise Theta1, Theta2 matrix parameters
# are unrolled to a vector nn_params and rolled when necessari. This way is important to
# call computeNumericalGradient.R.
input_layer_size  = 400;  # 20x20 Input Images of Digits
hidden_layer_size = 25;   # 25 hidden units
num_labels = 10;          # 10 labels, from 1 to 10   
                          # (note that we have mapped "0" to label 10)

# Load the weights into variables Theta1 and Theta2
data <- readMat("ex4weights.mat")

#vector containig unrolled Theta1 and Theta2
nn_params <- c(data$Theta1, data$Theta2) 

cat(" Theta1 and Theta2 loaded!\n")

cat("\n")
readline("Press ENTER to continue...")
cat("\014")   # Clear console.
rm(data) # Cleaning workspace

## ==== Part 3: Compute Cost (Feedforward) =====================================
##
##      instructions:  Implement the feedforward part of the neural network 
##                     that returns the cost only. We suggest implementing the 
##                     feedforward cost *without* regularization first so that 
##                     it will be easier for you to debug. Later, in part 4, you 
##                     will get to implement the regularized cost.
##
## =============================================================================

cat("***** EXERCISE 4 - PART 3: Compute Cost (Feedforward) *****\n\n") 
readline("Press ENTER to start...")

cat("\nFeedforward Using Neural Network ...\n")

# Weight regularization parameter (we set this to 0 here).
lambda <- 0

J <- nnCostFunction(input_layer_size, hidden_layer_size, num_labels, 
                    X, y, lambda)(nn_params)

cat("\nCost at parameters (loaded from ex4weights): ", J, "\n")
cat("(This value should be about 0.287629)\n")

cat("\n")
readline("Press ENTER to continue...")
cat("\014")   # Clear console.


## ==== Part 4: Implement Regularization =======================================
##
##      instructions: read part 3 instructions   
##
## =============================================================================

cat("***** EXERCISE 4 - PART 4: Implement Regularization *****\n\n") 
readline("Press ENTER to start...")

cat('\nChecking Cost Function (w/ Regularization) ... \n')

# Weight regularization parameter (we set this to 1 here).
lambda <- 1

J <- nnCostFunction(input_layer_size, hidden_layer_size, num_labels, 
                    X, y, lambda)(nn_params)

cat("\nCost with regularization at parameters (loaded from ex4weights): ", J, "\n")
cat("(this value should be about 0.383770)\n")

cat("\n")
readline("Press ENTER to continue...")
cat("\014")   # Clear console.


## ==== Part 5: Sigmoid Gradient ===============================================
##
##      instructions: implement the gradient for the sigmoid function.   
##
## =============================================================================

cat("***** EXERCISE 4 - PART 5: Sigmoid Gradient *****\n\n") 
readline("Press ENTER to start...")

cat('\nEvaluating sigmoid gradient...\n')

g <- sigmoidGradient(c(-1, -0.5, 0, 0.5, 1))
cat("Sigmoid gradient evaluated at [-1, -0.5, 0, 0.5, 1]: \n\n", g,"\n" )

cat("\n")
readline("Press ENTER to continue...")
cat("\014")   # Clear console.
rm(g) # Cleaning workspace

## ==== Part 6: Initializing Pameters ==========================================
##
##      instructions:   Start to implment a two layer neural network that 
##                      classifies digits. You will start by implementing a 
##                      function to initialize the weights of the neural network   
##
## =============================================================================

cat("***** EXERCISE 4 - PART 6: Initializing Pameters *****\n\n") 
readline("Press ENTER to start...")

cat("\nInitializing Neural Network Parameters ...")

initial_Theta1 <- randInitializeWeights(input_layer_size, hidden_layer_size)
initial_Theta2 <- randInitializeWeights(hidden_layer_size, num_labels)

# Unroll parameters
initial_nn_params <- c(initial_Theta1,initial_Theta2)

cat(" Done! \n\n")
readline("Press ENTER to continue...")
cat("\014")   # Clear console.
rm(randInitializeWeights, initial_Theta1, initial_Theta2) # Cleaning workspace

## ==== Part 7: Implement Backpropagation ======================================
##
##      instructions: proceed to implement the backpropagation algorithm for the 
##      neural network. You should add to the code you've written in 
##      nnCostFunction.R to return the partial derivatives of the parameters.  
##
## =============================================================================

cat("***** EXERCISE 4 - PART 7: Implement Backpropagation *****\n\n") 
readline("Press ENTER to start...")

cat("\nChecking Backpropagation... \n")

# Check gradients by running checkNNGradients
checkNNGradients() 

cat("\n")
readline("Press ENTER to continue...")
cat("\014")   # Clear console.

#   cost) # cleaning ws


## ==== Part 8: Training Neural Network ========================================
##
##      instructions:   Train a neural network. To train the neural network, use 
##                      "fincg", which is function which works similarly to "optim". 
##                      Recall that these advanced optimizers are able to train 
##                      our cost functions efficiently
##
## =============================================================================

cat("***** EXERCISE 4 - PART 8: Training the Neural Network *****\n\n") 
readline("Press ENTER to start...")

cat('\nTraining Neural Network... \n\n')

#  You should also try different values of lambda
lambda <- 1

# Create "short hand" for the cost function to be minimized
costFunction <- nnCostFunction(input_layer_size, hidden_layer_size, 
                               num_labels, X, y, lambda) #over nn_params

gradFunction <- nnGradFunction(input_layer_size, hidden_layer_size, 
                               num_labels, X, y, lambda) #over nn_params

# Now, costFunction and gradFunction are functions that take in only one argument (the
# neural network parameters)

# lbfgsb3 works like fmincg (octave/matlab) but fast.
library(lbfgsb3)

# After you have completed the assignment, change the maxit to a larger
# value to see how more training helps.
opt <- lbfgsb3_(initial_nn_params, 
                fn= costFunction, 
                gr= gradFunction,
                control = list(trace=1,maxit=50))

nn_params <- opt$prm
cost <- opt$f

## # Obtain Theta1 and Theta2 back from nn_params

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

cat("\n")
readline("Press ENTER to continue...")
cat("\014")   # Clear console.
rm(elements_Theta1, rows_Theta1, cols_Theta1,
   elements_Theta2, rows_Theta2, cols_Theta2) # Cleaning workspace

## ==== Part 9: Visualize Weights ==============================================
##
##      instructions: "visualize" what the neural network is learning by
##                     displaying the hidden units to see what features they are
##                      capturing in the data.  
##
## =============================================================================

cat("***** EXERCISE 4 - PART 9:  Visualize Weights *****\n\n") 
readline("Press ENTER to start...")

cat("\nVisualizing Neural Network... \n")

displayData(Theta1[, -1])  # except column 1. That's the meaning of "-" sign

cat("\n")
readline("Press ENTER to continue...")
cat("\014")   # Clear console.
dev.off()

## ==== Part 10: Implement Predict =============================================
##
##      instructions:   implement the "predict" function to use the
##                      neural network to predict the labels of the training 
##                      set. This lets you compute the training set accuracy.
##
## =============================================================================

cat("***** EXERCISE 4 - PART 10:  Implement Predict *****\n\n") 
readline("Press ENTER to start...")

pred <- predict(Theta1, Theta2, X)

cat("\nTraining Set Accuracy: ", mean(pred == y) * 100, "\n")

cat("\n")
readline("Press ENTER to continue...")
cat("\014")   # Clear console.

## ==== Part 11: EXTRA  ===================================
##
##      instructions:   
##
## =============================================================================

##  To give you an idea of the network's output, you can also run
##  through the examples one at the a time to see what it is predicting.
cat("***** EXERCISE 4 - PART 11: Predict Random handwritten example  *****\n\n") 
readline("Press ENTER to start...")

##  Randomly permute examples
m <- nrow(X)
rand_perm <- sample(m)

for (i in 1:m) {
        cat("\nDisplaying random handwritten example... ")
        displayData(X[rand_perm[i],])
        
        cat("it is a ", pred[rand_perm[i]], " !?")
        
        sc <- character()
        cat("\n\n")
        sc <- readline(prompt = "Press ENTER to continue or 'q' to quit...")
        
        if(sc == "q"){
                break
                
        }
        dev.off()
}
dev.off()
rm(sc, rand_perm, opt, nn_params, m, lambda, J, i, initial_nn_params,
   costFunction, displayData, gradFunction, lbfgsb3_, nnCostFunction, 
   nnGradFunction, predict, sigmoidGradient, checkNNGradients, 
   computeNumericalGradient, debugInitializeWeights, cost)   # Cleaning workspace
