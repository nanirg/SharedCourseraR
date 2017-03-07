#load data from .mat file

library('R.matlab')
data <- readMat('ex3data1.mat')
X <-data$X
y <- data$y

#image(X,y)
n_clases=10
lambda=10
D=cbind(1,X)
theta=matrix(0,dim(D)[2],1)

source("oneVsAll.R")

result=oneVsAll(D,y,theta,lambda,n_clases)