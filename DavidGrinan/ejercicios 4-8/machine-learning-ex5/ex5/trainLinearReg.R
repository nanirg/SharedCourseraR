trainLinearReg <- function(X,y,lambda){


	#transpose of a number, resutl is a 1by1 matrix
	if (class(X) == "numeric")
		X <- t(X)

	
	initial_theta = matrix(0,dim(X)[2],1)
	opt=optim(par=initial_theta,fn=linearRegCostFunction(X,y,lambda),gr=linearRegCostFunctionGrad(X,y,lambda),
      method="BFGS")

	trainLinearReg=opt$par
	





}