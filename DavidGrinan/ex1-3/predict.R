predict <- function(Xa,theta){
	m=dim(Xa)[1]
	z=Xa%*%theta
	source("sigmoid.R")
	h=sigmoid(z)
	pre=matrix(0,m,1)
	for(i in 1:m){

		if(h[i]>=0.5){ pre[i]=1}
		else{pre[i]=0}

	}
	predict=pre
	}
	