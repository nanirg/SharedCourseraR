#Ejercicio 4 
#Neural Networks BP algorithm

library('R.matlab')
data <- readMat('ex4data1.mat')
X <-data$X
y <- data$y
m <- dim(X)[1]
data2 <- readMat('ex4weights.mat')
Theta1 <- data2$Theta1
Theta2 <- data2$Theta2

#source("predictnn.R")
source("checkNNGradients.R")
source("debugInitializeWeights.R")
source("computeNumericalGradient.R")

input_layer_size  = 400
hidden_layer_size = 25
num_labels = 10

nn_params <-c(c(Theta1),c(Theta2))

lambda=1

source("nnCostFunction.R")
source("nnGradFunction.R")
coste=nnCostFunction(input_layer_size, hidden_layer_size, num_labels,
				X, y, lambda)(nn_params) 




################################################################################################









#Back Propagation

source("randIniWeigths.R")
initial_Theta1 <- randIniWeights(input_layer_size, hidden_layer_size)
initial_Theta2 <- randIniWeights(hidden_layer_size, num_labels)

# Unroll parameters
initial_nn_params <- c(initial_Theta1,initial_Theta2)

lambda <- 1

#checkNNGradients()

#obtain function inside
costFunction <- nnCostFunction(input_layer_size, hidden_layer_size, 
                                   num_labels, X, y, lambda) 

gradFunction <- nnGradFunction(input_layer_size, hidden_layer_size, 
num_labels, X, y, lambda) 



source("lbfgsb3_.R")

#######################  Regularization  #################

lambda <- 3
#checkNNGradients(lambda)

# Also output the costFunction debugging values
#debug_J  <- nnCostFunction(input_layer_size,hidden_layer_size, num_labels, X, y, lambda)(nn_params)

#cat(sprintf('\n\nCost at (fixed) debugging parameters (w/ lambda <- 3): %f (this value should be about 0.576051)\n\n', debug_J))








################################################################################################



#Training

source("lbfgsb3_.R")
library("lbfgsb3")
opt <- lbfgsb3_(initial_nn_params, fn= costFunction, gr=gradFunction,
control = list(trace=1,maxit=50))

nn_params=opt$prm

Theta1 <- matrix(nn_params[1:(hidden_layer_size * (input_layer_size + 1))],
                 hidden_layer_size, (input_layer_size + 1))

Theta2 <- matrix(nn_params[(1 + (hidden_layer_size * (input_layer_size + 1))):length(nn_params)],
num_labels, (hidden_layer_size + 1))






#Predict

source("predict.R")

pred <- predict(Theta1, Theta2, X)

bien=(pred==y)+0
bien2=sum(bien*bien)/m









