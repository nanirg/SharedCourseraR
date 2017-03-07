#exercise y pca

data <- readMat("ex7data1.mat")
X=data$X
plot(X[,1],X[,2],col='blue',xlim=c(.5,6.5),ylim=c(2,8))

#####  2

source("featureNormalize.R")
fN <- featureNormalize(X)
X_norm <- fN$X_norm
mu <- fN$mu
sigma <- fN$sigma


USV <- pca(X_norm)
U <- USV$u
S <- diag(USV$d)

#########  3
source("projectData.R")
source("recoverData.R")
K <- 1
Z <- projectData(X_norm, U, K)
X_rec <- recoverData(Z, U, K)

######## 4

data <- readMat("ex7faces.mat")

####### 5

fN <- featureNormalize(X)
X_norm <- fN$X_norm
mu <- fN$mu
sigma <- fN$sigma

#  Run PCA
USV <- pca(X_norm)
U <- USV$u
S <- diag(USV$d)


######## 6


K <- 100
Z <- projectData(X_norm, U, K)


K <- 100
X_rec <- recoverData(Z, U, K)






















