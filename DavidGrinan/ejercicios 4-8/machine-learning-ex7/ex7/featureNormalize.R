featureNormalize  <- function(X) {
  
  
  mu <- colMeans(X)
  

  X_norm <- matrix(mapply(`-`,t(X),mu),dim(X) ,byrow = TRUE)
  
  sigma <- apply(X,2,sd)
  X_norm <- matrix(mapply(`/`,t(X_norm),sigma),dim(X) ,byrow = TRUE)
  
  list(X_norm = X_norm, mu = mu, sigma = sigma)
  
}