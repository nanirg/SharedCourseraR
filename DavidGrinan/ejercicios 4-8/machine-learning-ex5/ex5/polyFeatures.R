polyFeatures <- function(X,p){



	m=dim(X)[1]
	res=matrix(0,m,p)
	for(i in 1:p){

		res[,i]=X^i
}

polyFeatures= res

}