projectData <- function(X, U, K) {
  
  Z <- matrix(0,dim(X)[1],K) 
  Z <- X %*% U[,1:K]
  Z
    
}