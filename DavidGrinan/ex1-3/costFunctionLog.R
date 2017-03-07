costFunctionLog <- function(Xa,y,theta){
	m=length(y)
	z=Xa%*%theta
	source("sigmoid.R")
	h=sigmoid(z)
	costFunctionLog=sum(-y*log(h)-(1-y)*log(1-h))/m
	}