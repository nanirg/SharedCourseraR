featurenormalice <- function(X){
	m=dim(X)[2]
	Xr=matrix(0,dim(X)[1],dim(X)[2])
	mus=matrix(0,1,m)
	sigmas=matrix(0,1,m)
	for(i in 1:m){

		mu=mean(X[,c(i)])
		sigma=sd(X[,c(i)])
		mus[i]=mu
		sigmas[i]=sigma
		Xr[,c(i)]=(X[,c(i)]-mu)/sigma
	}
	
	featurenormalice=list("nMatrix"=Xr,"mus"=mus,"sigmas"=sigmas)


}