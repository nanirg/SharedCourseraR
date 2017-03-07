findClosestCentroids <-function(X, centroids){

K <-dim(centroids)[1]
N <- dim(X)[2]
m <- dim(X)[1]
dist <- rep(0,K)
 a <- rep(0,N)
idx <- rep(0,dim(X)[1])
for (i in 1:m) {
    for (k in 1:K) {
      for (n in 1:N)	{
        a[n] <- (X[i,n] - centroids[k,n]) ^ 2	
      }
      dist[k] <- sum(a);		
    }
    c <- which.min(dist)		
    idx[i] <- c;				
  }


findClosestCentroids = idx


}