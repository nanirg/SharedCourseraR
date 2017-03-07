## Machine Learning Online Class
## Exercise 1B: Linear regression with multiple variables
## =============================================================================


## Initialization.
cat("\014")      # Clear console command = CTRL + L. 
rm(list = ls())  # Clear workspace command.

## linking the needed functions.
source("featureNormalize.R")
source("gradientDescentMulti.R")
source("normalEqn.R")

## ==== Part 1: Feature Normalization ==========================================
##
##      instructions: transform X_ij to X_ij = (X_ij - mu_j) / sigma_j
##
## =============================================================================

cat("***** Exercice 1B - Part 1: Feature Normalization *****\n") 
cat("\nPress ENTER to start...")
readline()

cat("Loading data ...\n\n")

## Load Data
data2 <- read.csv("ex1data2.txt", header = FALSE, 
                  col.names = c("house.size", "bedrooms", "house.price"))

## Print out some data points
cat("First 10 examples from the dataset:\n\n")

print(head(data2, n = 10))

## Scale features and set them to zero mean
cat("\nNormalizing Features ...   ")

## X features are the columns of data2 but the last which is y
X <- data2[,range(1,ncol(data2)-1)]
y <- data2[,ncol(data2)]

## Add x0 = 1 column to X features.
X <- data.frame(x0 = 1, X)

## Feature normalization
X_norm <- featureNormalize(X)[["X_norm"]]
mu <- featureNormalize(X)[["Mu"]]
sigma <- featureNormalize(X)[["Sigma"]]

cat(" done!\n") 
cat("\nPress ENTER to continue...")
readline()

cat("\014")   # Clear console.

## ==== Part 2: Gradient Descent ===============================================
##
##      instructions:   Compute the gradient descent algorithm with a 
##                      multivariate training set. Then estimate a house price.
##
## =============================================================================

cat("***** Exercice 1B - Part 2: Gradient Descent *****\n") 
cat("\nPress ENTER to start...")
readline()

cat("Running gradient descent ...\n\n")

## Choose some alpha value
alpha <- 0.1            
num_iters <- 400

## Init Theta and Run Gradient Descent 
theta <- rep(0, ncol(X_norm))

## Run gradient descent for multiple features.
gradDescResults <- gradientDescentMulti(X_norm, y, theta, alpha, num_iters)
theta <- gradDescResults$Theta
J_history <- gradDescResults$Jhistory

## Plot the convergence graph.
plot(1:num_iters, J_history,
     type = "l",
     col = "green",
     xlim = range(1,50),
     ylim = range(0,7e10),
     xlab = "number of iterations",
     ylab = "Cost J")

## Display gradient descent's result
cat("Theta computed from gradient descent: \n")
cat(theta)

## Estimate the price of a 1650 sq-ft, 3 br house
x_toPredict <- c(1, 1650, 3)
x_toPredict_norm <- (x_toPredict-mu)/sigma
x_toPredict_norm[1] <- 1

price <- sum(theta * x_toPredict_norm)

cat("\n\nPredicted price of a 1650 sq-ft, 3 br house (using gradient descent):\n")
cat("$ ", price)

cat("\n\nPress ENTER to continue...")
readline()

cat("\014")   # Clear console.

## ==== Part 3: Normal equation ================================================
##
##      instructions:   Computes the closed form solution for linear regression 
##                      using the normal equations. Then estimate house prices.
##
## =============================================================================

cat("***** Exercice 1B - Part 3: Normal equations *****\n") 
cat("\nPress ENTER to start...")
readline()

cat("Solving with normal equations...\n")

## Calculate the parameters from the normal equation
theta_normEq <- normalEqn(X,y)

## Display normal equation's result
cat("\nTheta computed from the normal equations: \n")
cat(theta_normEq)

## Estimate the price of the house
x0 <- rep(1,length(y))  # Add the x0 = 1 feature.
x_toPredict <- c(1, 1650, 3) 

price <- sum(theta_normEq*x_toPredict)

cat("\n\nPredicted price of a 1650 sq-ft, 3 br house (using normal equations):\n")
cat("$ ", price)

cat("\nPress ENTER to finish.")
readline()
dev.off()
