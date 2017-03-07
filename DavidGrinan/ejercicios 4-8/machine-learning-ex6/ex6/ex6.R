#exercise 6


##Plot
library('R.matlab')
data <- readMat('ex6data1.mat')
X <- data$X
y <- data$y
source("plotData.R")
plotData(X, y)

#part 2 training linear SVM
source("svmTrain.r")
source("visualizeBoundaryLinear.R")
source("linearKernel.R")
C=100
#Rprof()
model <- svmTrain(X, y, C, linearKernel, 1e-3, 20)
visualizeBoundaryLinear(X, y, model)


######Part3 Gaussian Kernel

source("gaussianKernel.R")

x1 <- c(1, 2, 1)
x2 <- c(0, 4, -1)
sigma <- 2
sim <- gaussianKernel(sigma)(x1, x2)

#cat(sprintf('Gaussian Kernel between x1 = [1; 2; 1], x2 = [0; 4; -1], sigma = 0.5 :\n\t%f\n(this value should be about 0.324652)\n', sim))

######Part4 Visualizing dataset 2

data <- readMat('ex6data2.mat')
X <- data$X
y <- data$y

plotData(X, y)

####Part 5 Training SVM with RBF Kernel

C <- 1; sigma <- 0.1
source("bsxfun.R")
source("visualizeBoundary.R")
source("meshgrid.R")
source("svmPredict.R")
model<- svmTrain(X, y, C, gaussianKernel(sigma))
q <- visualizeBoundary(X, y, model)



##Parte 6

data <- readMat('ex6data3.mat')
X <- data$X
y <- data$y
plotData(X, y)

source("dataset3Params.R")
d3P <- dataset3Params(X, y, Xval, yval)
C <- d3P$C #1
sigma <- d3P$sigma #0.1

# training
model<- svmTrain(X, y, C, gaussianKernel(sigma))
visualizeBoundary(X, y, model)




























