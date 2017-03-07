featureNormalize <- function(X){

mu=colMeans(X)
sigma=apply(X,2,sd)
X_norm <- matrix(mapply(`-`,t(X),mu),dim(X) ,byrow = TRUE)
X_norm <- matrix(mapply(`/`,t(X),sigma),dim(X) ,byrow = TRUE)


featureNormalize = list(X_norm=X_norm,mu=mu,sigma=sigma)


}