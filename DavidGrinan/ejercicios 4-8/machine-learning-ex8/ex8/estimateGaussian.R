estimateGaussian <- function(X){

m=dim(X)[1]
n=dim(X)[2]

mu=matrix(0,n,1)
sigma2=matrix(0,n,1)

sup=colSums(X)
mu=t(sup/m)

A=sweep(X,2,t(mu))
A=A*A
sigma2=colSums(A)*1/m

estimateGaussian=list(mu=mu,sigma2=sigma2)


}