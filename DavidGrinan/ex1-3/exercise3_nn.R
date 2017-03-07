#load data from .mat file

library('R.matlab')
data <- readMat('ex3data1.mat')
X <-data$X
y <- data$y
data2 <- readMat('ex3weights.mat')
th1 <- data2$Theta1
th2 <- data2$Theta2
source("predictnn.R")
p=predict(th1,th2,X)

#percentage
resultado=sum(0+(p==y))/5000
