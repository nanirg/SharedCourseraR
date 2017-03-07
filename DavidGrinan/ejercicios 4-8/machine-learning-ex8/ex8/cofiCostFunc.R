cofiCostFunc <- function(Y, R, num_users, num_movies,
                         num_features, lambda = 0) {
  function(params) {
    # Unfold 
    X <-
      matrix(params[1:(num_movies * num_features)], num_movies, num_features)
    Theta <-
      matrix(params[(num_movies * num_features + 1):length(params)],num_users, num_features)

    J <- 0
    J <- (1 / 2) * sum(((X %*% t(Theta)) * R - Y * R) ^ 2) +
      (lambda / 2 * sum(Theta ^ 2)) + (lambda / 2 * sum(X ^ 2))
    
    J
  }
}

cofiGradFunc <- function(Y, R, num_users, num_movies,
                         num_features, lambda = 0) {
  function(params) {
    X <-
      matrix(params[1:(num_movies * num_features)], num_movies, num_features)
    Theta <-
      matrix(params[(num_movies * num_features + 1):length(params)],
             num_users, num_features)
    
    
   
    X_grad <- matrix(0,dim(X)[1],dim(X)[2])
    Theta_grad <- matrix(0, dim(Theta)[1], dim(Theta)[2])
    
    X_grad <- (((X %*% t(Theta)) * R) %*% Theta - (Y * R) %*% Theta) + lambda * X
    Theta_grad <- t((t(X) %*% ((X %*% t(Theta)) * R) - t(X) %*% (Y * R))) + lambda * Theta
    
    grad <- c(c(X_grad),c(Theta_grad))
    grad
  }
}