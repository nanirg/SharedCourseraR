dataex2=read.csv("ex1data2.txt",header=FALSE)
X = dataex2[,c(1,2)]
y = dataex2[,c(3)]
m=dim(X)[2]
source("featurenormalice.R")

A=featurenormalice(X)