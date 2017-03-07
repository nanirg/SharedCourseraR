#Ejercicio 5 
#CV and Learning curve


#loading the data

library('R.matlab')
data <- readMat('ex5data1.mat')
X <- data$X
y <- data$y
Xtest <- data$Xtest
ytest <- data$ytest
Xval <- data$Xval
yval <- data$yval


##plot

m <- dim(X)[1]

plot(X, y, pch=4, lwd=2, col=2, xlab="change in water level (X)", 
ylab= "water flowing out of the dam (y)")

#Regularized cost function

#script linearRegCostFunction


theta <- c(1,1)

source("linearRegCostFunction.R")
source("linearRegCostFunctionGrad.R")


j=linearRegCostFunction(cbind(1,X), y, 1)(theta)
grad=linearRegCostFunctionGrad(cbind(1,X), y, 1)(theta)
#check OK


#Training Model

lambda=0
source("trainLinearReg.R")
theta= trainLinearReg(cbind(1,X), y, lambda)


plot(X, y, col="red", lwd=2, pch=4, 
     xlab="change in water level (x)",
     ylab="water flowing out of the dam (y)")

lines(X, cbind(1,X) %*% theta, lwd=2, col="blue")


# Learning curve 

lambda=0
source("learningCurve.R")
errores=learningCurve(cbind(1,X), y, cbind(1, Xval), yval, lambda)


error_train <- errores$etrain
error_val <- errores$eval



plot(c(1:m,1:m),c(error_train,error_val), type="n",main='Learning curve for linear regression',
     xlab="Number of training examples", ylab="Error")
lines(1:m, error_train, type="l",col="blue")
lines(1:m, error_val, type="l", col="green")
legend("topright",c("Train","Cross Validation"), 
col=c("blue","green"), lty=1)


### Poly Features
source("polyFeatures.R")
p = 8
X_pol <- polyFeatures(X, p)


## Feature Normalize  
source("featureNormalize.R")
lista= featureNormalize(X_pol)
X_poly=lista$X_norm
mu=lista$mu
sigma=lista$sigma

X_poly_test <- polyFeatures(Xtest, p)
X_poly_test <- matrix(mapply(`-`,t(X_poly_test),mu),dim(X_poly_test) ,byrow = TRUE)
X_poly_test <- matrix(mapply(`/`,t(X_poly_test),sigma),dim(X_poly_test) ,byrow = TRUE)
X_poly_test <- cbind(rep(1,dim(X_poly_test)[1]), X_poly_test)


X_poly_val <- polyFeatures(Xval, p)
X_poly_val <- matrix(mapply(`-`,t(X_poly_val),mu),dim(X_poly_val) ,byrow = TRUE)
X_poly_val <- matrix(mapply(`/`,t(X_poly_val),sigma),dim(X_poly_val) ,byrow = TRUE)
X_poly_val <- cbind(rep(1,dim(X_poly_val)[1]), X_poly_val)











