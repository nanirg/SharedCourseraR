#nnCostFunction
nnCostFunction <- function(input_layer_size, hidden_layer_size, num_labels,
				X, y, lambda) {

	function(nn_params){
	source("sigmoid.R")
	Theta1 <- matrix(nn_params[1:(hidden_layer_size * (input_layer_size + 1))],
	hidden_layer_size, (input_layer_size + 1))
      
	Theta2 <- matrix(nn_params[(1 + (hidden_layer_size * (input_layer_size + 1))):length(nn_params)],
	num_labels, (hidden_layer_size + 1))

	m <- dim(X)[1]
	J <- 0
	I <- diag(num_labels)
      Y <- matrix(0, m, num_labels)
      for (i in 1:m){
		Y[i,] <- I[y[i],]}


	#Feed forward
	a1 <- cbind(rep(1,m),X)
      z2 <- a1 %*% t(Theta1)
      a2 <- cbind(rep(1,dim(z2)[1]), sigmoid(z2))
      z3 <- a2 %*% t(Theta2)
      a3 <- sigmoid(z3)
	h <- a3
	

	#vectorization 
	
	p <- sum(Theta1[,-1] ^ 2) + sum(Theta2[,-1] ^ 2)
	J <- sum((-Y) * log(h) - (1 - Y) * log(1 - h)) / m + lambda * p / (2 * m)

	nnCostFunction=J
	

}}
