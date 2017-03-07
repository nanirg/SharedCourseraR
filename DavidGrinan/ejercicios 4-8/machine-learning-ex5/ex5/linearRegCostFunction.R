linearRegCostFunction <- function(X,y,lambda){


	function(theta){
		
		m=dim(X)[1]
		J=0
		h = X %*% theta
		thetas <- theta[-1]
		J <- 1/(2*m)*sum((h-y)^2)+(lambda/(2*m))*sum(thetas^2)
		linearRegCostFunction=J

}


}