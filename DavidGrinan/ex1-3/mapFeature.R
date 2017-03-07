mapFeature <-function(X1, X2){


	degree = 6
	out = matrix(0,length(X1),28)
	c=1
for (i in 1:degree){
	c=c+i
    for (j in 0:i){
        out[,c+j] = (X1^(i-j))*(X2^j)
	  
    }
	
}
out[,1]=matrix(1,length(X1))
mapFeature=out
}