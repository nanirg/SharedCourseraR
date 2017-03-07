pca <- function(X) {
 
  m <- dim(X)[1]
  n <- dim(X)[2]
  

  U <- rep(0,n)
  S <- rep(0,n)
  
 
  
  Sigma <- (1 / m) * (t(X) %*% X) #covariance matrix
  USV <- svd(Sigma)
  
  USV
   
}