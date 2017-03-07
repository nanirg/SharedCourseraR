gradientLog <- function(Xa,y,theta){
	m=length(y)
	z=Xa%*%theta
	source("sigmoid.R")
	h=sigmoid(z)
	X=t(Xa)
	gradientLog=(X%*%h)/m
	}