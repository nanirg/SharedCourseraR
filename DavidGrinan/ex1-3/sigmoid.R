sigmoid <-function(X){

	d=dim(X)
	if(is.null(d)){
		M=1/(1+exp(-X))
	}
	else{

		M=matrix(0,d[1],d[2])
		for(i in 1:d[1]){
			for(j in 1:d[2]){
			
				M[i,j]= 1/(1+exp(-X[i,j]))

			}
		}
	
	}
	sigmoid=M

}