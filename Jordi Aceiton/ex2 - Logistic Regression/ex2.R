## Machine Learning Online Class
## Exercise 2A: Logistic Regression
## =============================================================================


## Initialization.
cat("\014")      # Clear console command = CTRL + L. 
rm(list = ls())  # Clear workspace command.

## linking the needed functions.
source("costFunction.R")
source("plotDecisionBoundary.R")
source("plotData.R")

## Load Data
#  The first two columns contains the exam scores and the third column
#  contains the label.
data <- read.csv("ex2data1.txt", header = FALSE, 
                 col.names = c("results1","results2","admission"))

## ==== Part 1: Plot data ======================================================
##
##      instructions:   We start the exercise by first plotting the data to 
##                      understand the problem we are working with.
##
## =============================================================================

cat("***** EXERCISE 2A - PART 1: Plot data *****\n\n") 
readline("Press ENTER to start...")

## Setup the data matrix appropriately, and add ones for the intercept term.
X <- data.frame(x0=1, data[ ,1:ncol(data)-1])
X <- as.matrix(X)
y <- data[,ncol(data)]

## Plot data X, y
cat("\nPlotting data with (+) indicating (y = 1) examples and (o) indicating", 
    "(y = 0) examples.\n")

xy_labels <- c("Exam 1 score","Exam 2 score") 
legend_labels <- c("Admitted","not admitted")

plotData(X, y, xy_labels, legend_labels)

cat("\n")
readline("Press ENTER to continue...")

cat("\014")   # Clear console.
dev.off()

## ==== Part 2: Compute Cost and Gradient ======================================
##
##      instructions:   implement the cost and gradient for logistic regression.    
##                      
##
## =============================================================================

cat("***** EXERCISE 2A - PART 2: Compute Cost and Gradient *****\n\n") 
readline("Press ENTER to start...")

## Initialize fitting parameters
initial_theta <- rep(0, ncol(X))

## Compute and display initial cost and gradient
cost <- costFunction(initial_theta, X, y)[["J"]]
grad <- costFunction(initial_theta, X, y)[["Gradient"]]

cat("\nCost at initial theta (zeros): ", cost)
cat("\nExpected cost (approx): 0.693\n")
cat("Gradient at initial theta (zeros): ", grad, "\n")
cat("Expected gradients (approx): -0.1000 -12.0092 -11.2628\n")

## Compute and display cost and gradient with non-zero theta
test_theta <- c(-24,0.2,0.2)
cost <- costFunction(test_theta, X, y)[["J"]]
grad <- costFunction(test_theta, X, y)[["Gradient"]]

cat("\nCost at test theta: ", cost)
cat("\nExpected cost (approx): 0.218\n")
cat("Gradient at test theta: ", grad, "\n")
cat("Expected gradients (approx): 0.043 2.566 2.647\n")

cat("\n")
readline("Press ENTER to continue...")

cat("\014")   # Clear console.

## ==== Part 3: Optimizing using fminunc =======================================
##
##      instructions:   use a built-in function (fminunc) to find the optimal
##                      parameters theta.    
##                      
## =============================================================================

cat("***** EXERCISE 2A - PART 3: Optimizing using fminunc *****\n\n") 
readline("Press ENTER to start...")

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

optimRes <- optim(par = initial_theta,
                  fn = JcostFunction, X = X, y = y,  # assign explicitly the argument values.
                  gr = gradient, 
                  method="BFGS",
                  control = list(maxit = 400))

theta <- optimRes$par
cost <- optimRes$value

cat("\nCost at theta found by fminunc: ", cost, "\n")
cat("Expected cost (approx): 0.203\n")
cat("theta: ", theta, "\n")
cat("Expected theta (approx): -25.161  0.206  0.201\n")

## Plot boundary 
plotDecisionBoundary(theta, X, y, 
                     axLables = c("Exam 1 score","Exam 2 score"), 
                     legLabels = c('Admitted', 'Not admitted'))

cat("\n")
readline("Press ENTER to continue...")

cat("\014")   # Clear console.


## ==== Part 4: Predict and Accuracies =======================================
##
##      instructions:   use the logistic regression model to predict the 
##                      probability that a student with score 45 on exam 1 and 
##                      score 85 on exam 2 will be admitted.
##
##                      Furthermore, it computes the training and test set 
##                      accuracies of our model.    
##                      
## =============================================================================

cat("***** EXERCISE 2A - PART 4: Predict and Accuracies *****\n\n") 
readline("Press ENTER to start...")

sigmoid <- function(z) { 1/(1+exp(-z)) }
x_pred <- c(1, 45, 85)
z <- sum(theta * x_pred)
h <- sigmoid(z)

cat("\nFor a student with scores 45 and 85,\nwe predict an admission " ,
    "probability of: ", h, "\n");
cat("Expected value: 0.775 +/- 0.002\n");

##  Compute accuracy on our training set
## Predictions
pred <- as.numeric((sigmoid((X %*% theta )) >= 0.5))  

cat("\nTrain Accuracy: ", mean((pred == y)) * 100, "%\n")
cat('Expected accuracy (approx): 89.0\n')


cat("\n")
readline("Press ENTER to finish.")
dev.off()
