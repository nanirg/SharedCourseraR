library("R.matlab")

data <- readMat("ex8data1.mat")
m=dim(X)[1]
X <- data$X
Xval <-data$Xval
yval=data$yval
plot(X[,1], X[,2], xlim=c(0,30),ylim=c(0,30), pch=4, col="blue",
     xlab='Latency (ms)', ylab='Throughput (mb/s)')


######### parte 2
source("estimateGaussian.R")

a=estimateGaussian(X)

mu=a$mu
sigma2=a$sigma2
source("bsxfun.R")
source("multivariateGaussian.R")
source("pinv.R")

p <- multivariateGaussian(X, mu, sigma2)

########outliers

pval <- multivariateGaussian(Xval, mu, sigma2)

source("selectThreshold.R")
sT <- selectThreshold(yval, pval)
epsilon <- sT$bestEpsilon
F1 <- sT$bestF1
outliers = which(p < epsilon)

#########Multidim outliers

data <- readMat("ex8data2.mat")

X <- data$X
Xval <-data$Xval
yval=data$yval
eG <- estimateGaussian(X)
mu <- eG$mu
sigma2 <- eG$sigma2
p <- multivariateGaussian(X, mu, sigma2)

pval <- multivariateGaussian(Xval, mu, sigma2)

sT <- selectThreshold(yval, pval)
epsilon <- sT$bestEpsilon
F1 <- sT$bestF1




