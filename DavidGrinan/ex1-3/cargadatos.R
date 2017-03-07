dataex1=read.csv("ex1data1.txt",header=TRUE)
X = dataex1[,c(1)]
y = dataex1[,c(2)]

plot(X,y,ylab="Profit in $10,000s",xlab="Population of City in 10,000s")


m=length(X)
X2 = cbind(1,X)
n=dim(X2)[2]
theta = matrix(0,n,1)
it=1500
alpha=0.01

source("coste.r")
coste=J(X2,y,theta)


costes=matrix(0,it,1)

for(i in 1:it){
	
	h=X2%*%theta
	h=t(h)
	th0=h-y
	th1=th0%*%X
	upd0=(alpha/m)*sum(th0)
	upd1=(alpha/m)*sum(th1)
	theta[1,1]=theta[1,1]-upd0
	theta[2,1]=theta[2,1]-upd1
	costes[i,1]=J(X2,y,theta)
}


theta0_vals=seq(-10,10,length.out=100)
theta1_vals=seq(-10,10,length.out=100)
j_vals=matrix(0,100,100)
t=theta
for(i in 1:100){

	for(j in 1:100){
		
		t[1,1]=theta0_vals[i]
		t[2,1]=theta1_vals[j]
		j_vals[i,j]=J(X2,y,t)

}

}

persp(theta0_vals,theta1_vals,j_vals,phi=30,theta=75)
contour(theta0_vals,theta1_vals,j_vals,nlevels=200)
