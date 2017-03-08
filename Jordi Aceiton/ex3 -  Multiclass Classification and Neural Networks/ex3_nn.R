## Machine Learning Online Class
## Exercise 3 | Part 2: Neural Networks
## =============================================================================

## Initialization.
rm(list = ls())  # Clear workspace command.
cat("\014")      # Clear console.

## linking the needed functions.
source("displayData.R")
source("predict.R")

## ==== Part 1: Loading and visualizing data ===================================
##
##      instructions:   loading and visualizing the dataset that contains 
##                      handwritten digits.
##
## =============================================================================

## Setup the parameters you will use for this exercise
input_layer_size  <- 400  # 20x20 Input Images of Digits
hidden_layer_size <- 25   # 25 hidden units
num_labels <- 10          # 10 labels, from 1 to 10   
                          # (note that we have mapped "0" to label 10)

## load the data with R.matlab package to read .mat files
suppressWarnings(
        
        if (require(R.matlab) == FALSE) {
                stop("\n\n Install R.matlab package To run this script")
        } else {
                cat("\014")  # else Clear console
        }      
)

cat("***** EXERCISE 3B - PART 1: Loading and visualizing data *****\n\n") 
readline("Press ENTER to start...")

data <- readMat("ex3data1.mat")
X <- data$X
y <- data$y
rm(data)


## Randomly select 100 data points to display:
rand_indices <- sample(nrow(X))
rand_X_selection <- X[rand_indices[1:100], ]

## Call displayData() function sourced by displayData.R file.
cat("\nLoading data and plotting 100 random examples...\n")
displayData(rand_X_selection)

cat("\n")
readline("Press ENTER to continue...")

cat("\014")   # Clear console.

## ==== Part 2: Loading Parameters ===============================================
##
##      instructions:   load some pre-initialized neural network parameters
##
## =============================================================================

cat("***** EXERCISE 3B - PART 2: Loading Parameters *****\n\n") 
readline("Press ENTER to start...")

cat("\nLoading Saved Neural Network Parameters ... ")

## Loading Theta1 and Theta2 parameters
data <- readMat("ex3weights.mat")
Theta1 <- data$Theta1
Theta2 <- data$Theta2
rm(data)

cat("Theta1 and Theta2 loaded!\n")

cat("\n")
readline("Press ENTER to continue...")

detach("package:R.matlab")
cat("\014")   # Clear console.

## ==== Part 3: Implement Predict ===============================================
##
##      instructions:   use the loaded optimized parameters to predict the  
##                      labels. This lets you compute the training set accuracy.
##
## =============================================================================

cat("***** EXERCISE 3B - PART 3: Implement Predict *****\n\n") 
readline("Press ENTER to start...")

pred <- predict(Theta1, Theta2, X)

cat("\nTraining Set Accuracy: ", mean(pred == y) * 100, "%.\n")


cat("\n")
readline("Press ENTER to continue...")

dev.off()
cat("\014")   # Clear console.


##  To give you an idea of the network's output, you can also run
##  through the examples one at the a time to see what it is predicting.
cat("***** EXERCISE 3B - PART 4: Predict random handwritten example *****\n\n") 
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

