recoverData <- function(Z, U, K) {
  X_rec <- matrix(0,dim(Z)[1],dim(U)[1])
  X_rec <- Z %*% t(U[,1:K])
  X_rec
 
  
}