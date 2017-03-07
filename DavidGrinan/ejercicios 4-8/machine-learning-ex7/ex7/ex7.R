#esercise 7

library("R.matlab")

data <- readMat("ex7data2.mat")

X <- data$X
source("findClosestCentroids.R")
K <- 3
initial_centroids <- matrix(c(3, 3, 6, 2, 8, 5),3,2,byrow = TRUE)
idx <- findClosestCentroids(X, initial_centroids)

######Compute Means
source("computeCentroids.R")
centroids = computeCentroids(X, idx, K);

####K means clustering

K <- 3
max_iters <- 10
source("runkMeans.R")
kMean <- runkMeans(X, initial_centroids, max_iters, FALSE)
centroids <- kMean$centriods
idx <- kMean$idx

##########pixel

data <- readMat("bird_small.mat")

A=data$A
A=A/255
img_size=dim(A)
X <- matrix(A, img_size[1] * img_size[2], 3)
K <- 16
max_iters <- 10

source("kMeansInitCentroids.R")

initial_centroids <- kMeansInitCentroids(X, K)
kMean <- runkMeans(X, initial_centroids, max_iters)
centroids <- kMean$centroids
idx <- kMean$idx


########compress

#########




