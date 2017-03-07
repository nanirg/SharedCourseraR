#Load the data

dataex2=read.csv("ex2data2.txt",header=FALSE)
X = dataex2[,c(1)]
Y = dataex2[,c(2)]
lab= dataex2[,c(3)]
D = matrix(0,length(Y),2)
D = dataex2[,c(1,2)]
D=as.matrix(D)

#Plot the data 

lab=lab+1
#plot(X,Y,pch=23,bg=c("red","blue")[lab])
source("mapFeature.R")
lab=lab-1

#test matrices for mapFeature
#x=matrix(2,28,1)
#y=matrix(3,28,1)

featmap=mapFeature(X,Y)

lambda_ini=10
theta_ini=matrix(0,28,1)
source("costFunctionLogReg.R")
coste=costFunctionLogReg(featmap,lab,theta_ini,lambda_ini)

source("gradientLogReg.R")
a = optim(par=theta_ini,fn=costFunctionLogReg,gr=gradientLogReg,Xa=featmap,y=lab,lambda=lambda_ini)


