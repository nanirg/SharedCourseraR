oneVsAll <- function(X,y,theta,lambda_ini,n_clases){

source("costFunctionLogReg.R")
source("gradientLogReg.R")
D=X
lab=matrix(0,length(y),1)
thetas=matrix(0,dim(D)[2],n_clases)
for(i in 1:n_clases){
	lab=(y==i)+0
	a = optim(par=theta_ini,fn=costFunctionLogReg,gr=gradientLogReg,Xa=D,y=lab,lambda=lambda_ini)
	thetas[,i]=a$par


	}	
oneVsAll=matrix(0,dim(D)[2],n_clases)
oneVsAll=thetas


}