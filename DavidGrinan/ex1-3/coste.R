J <- function(X,y,theta){

	
	h=X%*%theta
	m=length(y)
	r=h-y
	r2=r*r
	suma=sum(r2)
	J=suma/(2*m)



}