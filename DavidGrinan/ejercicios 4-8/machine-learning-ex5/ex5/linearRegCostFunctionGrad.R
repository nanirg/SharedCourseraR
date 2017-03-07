linearRegCostFunctionGrad <- function(X,y,lambda){


	function(theta){
		
		m=dim(X)[1]
		grad = matrix(0,length(theta),1)
		h = X %*% theta
		theta2 <- theta
		theta2[1] <- 0
		grad= (1/m)*(t(X)%*%(h-y) +lambda*theta2)
		linearRegCostFunctionGrad=grad

}


}