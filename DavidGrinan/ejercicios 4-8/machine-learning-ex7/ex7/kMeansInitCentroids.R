kMeansInitCentroids <- function(X,K){

centroids <- matrix(0,K,dim(X)[2])
randidx <- sample(dim(X)[1])  
centroids <- X[randidx[1:K],]
kMeansInitCentroids = centroids



}