gradientLogReg <- function(Xa,y,theta,lambda){
	m=length(y)
	z=Xa%*%theta
	source("sigmoid.R")
	h=sigmoid(z)
	X=t(Xa)
	pre=(X%*%h)/m +lambda/m*theta
	pre[1]=pre[1]-lambda/m*theta[1]
	gradientLogReg=pre
	}