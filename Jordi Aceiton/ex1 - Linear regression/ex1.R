## Machine Learning Online Class
## Exercise 1A: Linear Regression
## =============================================================================


## Initialization.
cat("\014")      # Clear console command = CTRL + L. 
rm(list = ls())  # Clear workspace command.

## load the needed functions.
source("warmUpExercise.R")
source("plotData.R")
source("computeCost.R")
source("gradientDescent.R")


## ==== Part 1: Basic Function =================================================
##
##      Instructions: Return the 5x5 identity matrix 
##
## *****************************************************************************

cat("***** Exercice 1A - Part 1: Basic Function *****\n") 
cat("\nPress ENTER to start...")
readline()

cat("Running warmUpExercise ...  \n")

n <- 5
identityMatrix <- warmUpExercise(n)

cat(paste0("\n", n, "x", n, " Identity matrix"), "\n\n")
print(identityMatrix)

cat("\nPress ENTER to continue...")
readline()

rm(n)
cat("\014")   # Clear console.


## ==== Part 2: Plotting =======================================================
##
##     Instructions: Plot the training data into a figure. 
##                   
##                   Set the axes labels using the "xlabel" and "ylabel" 
##                   commands. Assume the population and revenue data have been 
##                   passed in as the x and y arguments of this function. 
##
## *****************************************************************************

cat("***** Exercice 1A - Part 2: Plotting *****\n") 
cat("\nPress ENTER to start...")
readline()

cat("Plotting Data ...\n")

# Training set.
data1 <- read.csv("ex1data1.txt", header=FALSE, col.names = c("X","y"))
X <- data1$X
y <- data1$y

# Plot the data.
plotData(X, y)

cat("\nPress ENTER to continue...")
readline()
dev.off()

cat("\014")   # Clear console.


## ==== Part 3: Gradient descent =======================================================
##
##     Instructions: Perform a single gradient step on the parameter vector
##                   theta.
##
## =====================================================================================

cat("***** Exercice 1A - Part 3: Gradient descent *****\n") 
cat("\nPress ENTER to start...")
readline()

cat("Running Gradient Descent ... \n")

# Add the x0 = 1 column to X.
X <- matrix(1, nrow=nrow(data1), ncol=2)
X[,2] <- data1$X

# Initialize theta parameters
theta <- rep(0, ncol(X))

# Gradient descent settings
iterations <- 1500
alpha <- 0.01

# Compute and display initial cost
Jini <- computeCost(X, y, theta)

cat("\nInitial cost J(0,0) = ", Jini, "\n\n")

# Run gradient descent.
gradDescResults <- gradientDescent(X, y, theta, alpha, iterations)
theta <- gradDescResults$Theta
J_history <- gradDescResults$Jhistory


cat("Theta found by gradient descent: theta = ", theta, "\n\n")

# Plot the linear fit
plotData(X[,2],y)
lines(X[,2], X%*%theta, col="blue")

# Predict values for population sizes of 35,000 and 70,000
predict1 <- sum(c(1,3.5)*theta) * 10000
predict2 <- sum(c(1,7)*theta) * 10000

cat("For population = 35,000, we predict a profit of ", predict1,"\n")
cat("For population = 70,000, we predict a profit of ", predict2,"\n")

cat("\nPress ENTER to continue...")
readline()
dev.off()

cat("\014")   # Clear console.

## ==== Part 4:  Visualizing J(theta_0, theta_1) ===============================
##
##     Instructions: Visualize J using a 3d surface plot and a contour plot. In 
##                   the last plot show the the theta point which adjust the 
##                   data the best found by gradient descent algorithm.
##
## =============================================================================

cat("***** Exercice 1A - Part 4: Visualizing J(theta_0, theta_1) *****\n") 
cat("\nPress ENTER to start...")
readline()

cat("Visualizing J(theta_0, theta_1) surface plot...\n")

# Grid over which we will calculate J
theta0_val <- seq(-10, 10, length.out=100)
theta1_val <- seq(-1, 4, length.out=100)

# Initialize J_vals to a matrix of 0's
J_vals <- matrix(0, nrow=length(theta0_val), ncol=length(theta1_val))

# Fill out J_vals
for (i in 1:length(theta0_val)) {
        for (j in 1:length(theta1_val)) {
                thta <- c(theta0_val[i], theta1_val[j])
                J_vals[i,j] <- computeCost(X, y, thta)
        }
}
rm(thta)

## Setting surface plot.
par(bg = "white")

x <- theta0_val
y <- theta1_val
z <- J_vals

# Create a function interpolating colors in the range of specified colors
jet.colors <- colorRampPalette( c("blue","lightblue", "green", "yellow", "red") )

# Generate the desired number of colors from this palette
nbcol <- 100
color <- jet.colors(nbcol)

# Compute the z-value at the facet centres
nrz <- nrow(z)
ncz <- ncol(z)
zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]

# Recode facet z-values into color indices
facetcol <- cut(zfacet, nbcol)

# Plot the surface graphic.
persp(x, y, z,
      ticktype = "detailed",
      col = color[facetcol], 
      phi = 30, 
      theta = -30,
      main = "J( theta0, theta1) cost function",
      xlab = "theta0",
      ylab = "theta1",
      zlab = "",
      zlim = range(0,800,200))

cat("\nPress ENTER to continue...")
readline()
dev.off()

## Contour plot

cat("Visualizing J(theta_0, theta_1) contour plot...\n")
cat("The red cross (theta0,theta1) stands for the optimal J obtained by gradient descent.\n")

x <- theta0_val
y <- theta1_val
z <- J_vals

lev <- c(8, 15, 30, 50, 100, 200, 300, 450, 600)
contour(x , y, z,
        levels = lev)
points(theta[1], theta[2], col = "red", pch = 4)

cat("\nPress ENTER to finish.")
readline()
dev.off()

