predict <- function(Theta1, Theta2, X) {

  if (is.vector(X))
    X <- t(X)
  
  m <- dim(X)[1]
  num_labels <- dim(Theta2)[1]
  
  
  p <- rep(0,m)
  
  
  
  a1 <- cbind(rep(1,m), X)
  z2 <- a1 %*% t(Theta1)
  a2 <- cbind(rep(1,dim(z2)[1]) ,sigmoid(z2))
  z3 <- a2 %*% t(Theta2)
  a3 <- sigmoid(z3)
  
  p <- apply(a3,1,which.max)
  p
 }