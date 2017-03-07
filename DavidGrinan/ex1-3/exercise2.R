#Load the data

dataex2=read.csv("ex2data1.txt",header=FALSE)
X = dataex2[,c(1)]
Y = dataex2[,c(2)]
lab= dataex2[,c(3)]
D = matrix(0,length(Y),2)
D = dataex2[,c(1,2)]
D=as.matrix(D)

#Plot the data 

lab=lab+1
plot(X,Y,pch=23,bg=c("red","blue")[lab])

#Logistic Regression

D=cbind(1,D)
lab=lab-1
theta_ini=matrix(0,3,1)
source("costFunctionLog.R")
coste=costFunctionLog(D,lab,theta_ini)

source("gradientLog.R")
a = optim(par=theta_ini,fn=costFunctionLog,gr=gradientLog,Xa=D,y=lab)
source("predict.R")
fin=predict(D,a$par)

#ratio of misclassification

err=lab-fin
res=sum(err*err)/length(err)





