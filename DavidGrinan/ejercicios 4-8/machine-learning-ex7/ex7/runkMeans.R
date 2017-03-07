runkMeans <-function(X, initial_centroids,max_iters, plot_progress = FALSE){


  m <- dim(X)[1]
  n <- dim(X)[2]
  K <- dim(initial_centroids)[1]
  centroids <- initial_centroids
  previous_centroids <- array(0,dim = c(dim(centroids),max_iters + 1))
  previous_centroids[,,1] <- centroids
  idx <- rep(0,m)
  for (i in 1:max_iters) {
    idx <- findClosestCentroids(X, centroids)
    centroids <- computeCentroids(X, idx, K)
    previous_centroids[,,i + 1] <- centroids
  }
  
list(centroids = centroids, idx = idx)



}