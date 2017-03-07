dataex2=read.csv("ex1data2.txt",header=FALSE)
X = dataex2[,c(1,2)]
y = dataex2[,c(3)]
m=dim(X)[1]
n=dim(X)[2]
source("featurenormalice.R")

A=featurenormalice(X)

XR=A$nMatrix
mus=A$mus
sigmas=A$sigmas

X2 = cbind(1,XR)
theta = matrix(0,n+1,1)
it=400
alpha=0.1

source("coste.r")



costes=matrix(0,it,1)

for(i in 1:it){
	
	h=X2%*%theta
	err=h-y
	err=t(err)
	for(j in 1:n+1){
		thj=err%*%X2[,j]
		updj=(alpha/m)*sum(thj)
		theta[j,1]=theta[j,1]-updj
	}
	costes[i,1]=J(X2,y,theta)
}


plot(seq(1,400,1),costes)
