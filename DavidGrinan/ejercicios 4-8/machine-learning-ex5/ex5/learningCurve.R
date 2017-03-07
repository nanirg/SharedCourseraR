learningCurve <- function(X,y,Xval,yval,lambda){

	m= dim(X)[1]

	etrain <- rep(0,m)
	eval <- rep(0,m)

	for(i in 1:m){
	
		theta <- trainLinearReg(X[1:i,],y[1:i,],lambda)
		h = X[1:i,] %*% theta
		etrain[i]<-(1/(2*i))*sum((h - y[1:i,])^2)
		h=Xval %*% theta
		eval[i]=(1/(2*m))*sum((h-yval)^2)
}

	trainLinearReg = list(etrain = etrain, eval=eval)




}