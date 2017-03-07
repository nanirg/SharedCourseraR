costFunctionLogReg <- function(Xa,y,theta,lambda){
	m=length(y)
	z=Xa%*%theta
	source("sigmoid.R")
	h=sigmoid(z)
	reg=lambda/(2*m)*(sum(theta*theta)-theta[1])
	costFunctionLog=sum(-y*log(h)-(1-y)*log(1-h))/m + reg
	}