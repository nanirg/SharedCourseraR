predict <- function(Theta1, Theta2, X) {
	source("sigmoid.R")
	m <- dim(X)[1]
	num_labels <- dim(Theta2)[1] 
	p=matrix(0,m,1)
	a1=cbind(1,X)
	z2=a1%*%t(Theta1)
	a2=cbind(1,sigmoid(z2))
	z3=a2%*%t(Theta2)
	a3=sigmoid(z3)
	predict= apply(a3,1,which.max)

 }