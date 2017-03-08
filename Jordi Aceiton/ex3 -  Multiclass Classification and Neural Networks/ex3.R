## Machine Learning Online Class
## Exercise 3A | Part 1: One-vs-all
## =============================================================================

## Initialization.
rm(list = ls())  # Clear workspace command.
cat("\014")   # Clear console.

## linking the needed functions.
source("displayData.R")
source("lrCostFunction.R")
source("lrGradient.R")
source("oneVsAll.R")
source("predictOneVsAll.R")

## ==== Part 1: Loading and visualizing data ===================================
##
##      instructions:   loading and visualizing the dataset that contains 
##                      handwritten digits.
##
## =============================================================================

## load the data with R.matlab package to read .mat files
suppressWarnings(
        
        if (require(R.matlab) == FALSE) {
                stop("\n\n Install R.matlab package To run this script")
        } else {
                cat("\014")  # else Clear console
        }      
)

cat("***** EXERCISE 3A - PART 1: Loading and visualizing data *****\n\n") 
readline("Press ENTER to start...")

data <- readMat("ex3data1.mat")
        X <- data$X
        y <- data$y
rm(data)
detach("package:R.matlab")

## Setup the parameters you will use for this part of the exercise
input_layer_size  <- 400        # 20x20 Input Images of Digits
num_labels <- 10                # 10 labels, from 1 to 10   
                                # (note that we have mapped "0" to label 10)

## Randomly select 100 data points to display:
rand_indices <- sample(nrow(X))
rand_X_selection <- X[rand_indices[1:100], ]

## Call displayData() function sourced by displayData.R file.
cat("\nLoading data and plotting 100 random examples...\n")
displayData(rand_X_selection)

cat("\n")
readline("Press ENTER to continue...")

cat("\014")   # Clear console.

## ==== Part 2: Vectorize logistic Regression ==================================
##
##      instructions:  Make sure that your regularized logistic regression
##                     implementation is vectorized. After that, implement 
##                     one-vs-all classification for the handwritten digit 
##                     dataset.        
##
## =============================================================================

cat("***** EXERCISE 3A - PART 2:  Vectorize logistic Regression *****\n\n") 
readline("Press ENTER to start...")

## Test case for lrCostFunction
cat("\nTesting lrCostFunction()...\n")

theta_t <- c(-2,-1,1,2)
X_t <- matrix(c(rep(1,5), (1:15)/10), nrow = 5, ncol = 4)
y_t <- c(1,0,1,0,1)
lambda_t <- 3
 
J <- lrCostFunction(theta_t, X_t, y_t, lambda_t)
grad <- lrGradient(theta_t, X_t, y_t, lambda_t)

cat("\nCost: ", J, "\n") 
cat("Expected cost: 2.534819\n\n") 
cat("Gradients: ", grad, "\n") 
cat("Expected gradients: 0.146561 -0.548558 0.724722 1.398003\n") 

cat("\n")
readline("Press ENTER to continue...")

cat("\014")   # Clear console.

## ==== Part 3: One-vs-All Training ============================================
##
##      instructions:  Make sure that your regularized logistic regression
##                     implementation is vectorized. After that, implement 
##                     one-vs-all classification for the handwritten digit 
##                     dataset.        
##
## =============================================================================

cat("***** EXERCISE 3A - PART 3:  One-vs-All Training *****\n\n") 
readline("Press ENTER to start...")

lambda <- 0.1

cat("\n")
all_theta <- oneVsAll(X, y, num_labels, lambda)

cat("\n")
readline("Press ENTER to continue...")

cat("\014")   # Clear console.


## ==== Part 4: Predict for One-Vs-All =========================================
##
##      instructions:         
##
## =============================================================================

cat("***** EXERCISE 3A - PART 4:  Predict for One-Vs-All *****\n\n") 
readline("Press ENTER to start...")

pred <- predictOneVsAll(all_theta, X)

cat("\nTraining Set Accuracy: ", mean((pred == y)) * 100, "\n")

cat("\n")
readline("Press ENTER to finish.")
dev.off()
