computeCentroids <- function(X,idx,K){

m <- dim(X)[1]
n <- dim(X)[2]
  
centroids <- matrix(0,K,n)



  for (k in 1:K) {
    num_k <- 0
    sum <- rep(0,n)
    for (i in 1:m) {
      if (idx[i] == k) {
        sum <- sum + t(X[i,])
        num_k <- num_k + 1
      }
    }
    centroids[k,] <- t(sum / num_k)
}


computeCentroids = centroids

}