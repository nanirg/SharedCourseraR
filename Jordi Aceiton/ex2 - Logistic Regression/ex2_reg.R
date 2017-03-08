## Machine Learning Online Class
## Exercise 2: Regularized Logistic Regression
## =============================================================================

## Initialization.
cat("\014")      # Clear console command = CTRL + L. 
rm(list = ls())  # Clear workspace command.

## linking the needed functions.
source("plotData.R")
source("mapFeature.R")
source("costFunctionReg.R")
source("plotDecisionBoundary.R")


## Load Data
#  The first two columns contains the test scores in microchips quality assurance
#  and the third column contains the label wether it is accepted or rejected.
data <- read.csv("ex2data2.txt", header = FALSE, 
                 col.names = c("test1","test2","accepted"))

## Setup the data matrix appropriately, and add ones for the intercept term.
X <- data.frame(x0=1, data[ ,1:ncol(data)-1])
X <- as.matrix(X)
y <- data[,ncol(data)]

## ==== Part 1: Plot data ======================================================
##
##      instructions:   you are given a dataset with data points that are not
##                      linearly separable. However, you would still like to use  
##                      logistic regression to classify the data points. 
##
##                      To do so, you introduce more features to use -- in 
##                      particular, you addpolynomial features to our data matrix
##                      (similar to polynomial regression).
##
## =============================================================================

cat("***** EXERCISE 2B - PART 1: Regularized Logistic Regression *****\n\n") 
readline("Press ENTER to start...")

## Visualise data
cat("\nPlotting the data...\n\n")
xy_labels <- c("Microchip Test 1","Microchip Test 2") 
legend_labels <- c("y = 1","y = 0")

plotData(X, y, xy_labels, legend_labels)

## Add Polynomial Features

# Note that mapFeature also adds a column of ones for us, so the intercept
# term is handled
X <- mapFeature(X[,2], X[,3])

## Initialize fitting parameters
initial_theta = rep(0, ncol(X))

## Set regularization parameter lambda to 1
lambda = 1

## Compute and display initial cost and gradient for regularized logistic
## regression
cost <- costFunctionReg(initial_theta, X, y, lambda)[["J"]]
grad <- costFunctionReg(initial_theta, X, y, lambda)[["Gradient"]]


cat("Cost at initial theta (zeros): ", cost, "\n")
cat("Expected cost (approx): 0.693\n\n")
cat("Gradient at initial theta (zeros) - first five values only:\n")
cat(grad[1:5], "\n\n");
cat("Expected gradients (approx) - first five values only:\n")
cat(" 0.0085  0.0188  0.0001  0.0503  0.0115\n")

cat("\n")
readline("Press ENTER to continue...")

cat("\014")   # Clear console.

## ==== Part 2: Regularization and Accuracies ==================================
##
##      instructions:     
##                      
## =============================================================================

cat("***** EXERCISE 2B - PART 2: Regularization and Accuracies *****\n\n") 
readline("Press ENTER to start...")

## Initialize fitting parameters
initial_theta = rep(0, ncol(X))

## Set regularization parameter lambda to 1
lambda = 1

## Optimize

## What is the R equivalent of Matlab's fminunc function?
## Take a look at the optim function. It can do unconstrained minimization using
## method = 'L-BFGS-B' and you can specify an analytical function to compute the 
## gradient as well
##
## EDIT. As Ben has pointed out correctly, fminunc does unconstrained optimization, 
## which can also be achieved using the optim function choosing Nelder-Mead or 
## BFGS. Moreover, I also noticed from the documentation of fminunc that it does
## large-scale optimization using trust region methods. There is an R package trust 
## that I believe does the same thing. I would recommend taking a look at the 
## optimization task view of R.

## Equivalent fminunc (optimitzation solver that finds de minimum of an
## unconstrained function.) in R using optim().

cat("\nRunning optimitzation and plotting decision boundary...\n")

optimRes <- optim(par = initial_theta,
                  fn = JcostFunctionReg, 
                  X = X, y = y, lambda = lambda,  # assign explicitly the argument values.
                  gr = gradientReg, 
                  method="BFGS",
                  control = list(maxit = 400))

theta <- optimRes$par
cost <- optimRes$value

## Plot boundary 
plotDecisionBoundary(theta, X, y, 
                     axLables = c("Exam 1 score","Exam 2 score"), 
                     legLabels = c('Admitted', 'Not admitted'))

## Compute accuracy on our training set
sigmoid <- function(z) { 1/(1+exp(-z)) }

## Predictions
pred <- as.numeric((sigmoid((X %*% theta )) >= 0.5))  

cat("\nTrain Accuracy: ", mean((pred == y)) * 100, "%\n")
cat('Expected accuracy (approx): 83.1 when lambda = 1\n')


cat("\n")
readline("Press ENTER to finish.")
dev.off()
