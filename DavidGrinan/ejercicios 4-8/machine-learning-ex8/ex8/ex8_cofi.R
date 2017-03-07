data <- readMat("ex8_movies.mat")
Y=data$Y
R=data$R


#collaborative filtering cost function

data2 <- readMat("ex8_movieParams.mat")
#X, Theta, num_users, num_movies, num_features)
X=data2$X
Theta=data2$Theta
num_users=data2$num_users
num_movies=data2$num_movies
num_features=data2$num_features

#reducing data

num_users <- 4; num_movies <- 5; num_features <- 3
X <- X[1:num_movies, 1:num_features]
Theta <- Theta[1:num_users, 1:num_features]
Y <- Y[1:num_movies, 1:num_users]
R <- R[1:num_movies, 1:num_users]

source("cofiCostFunc.R")

J <- cofiCostFunc( Y, R, num_users, num_movies,
num_features, 0)(c(c(X),c(Theta)))

######collaborative Filtering Gradient
source("checkCostFunction.R")
source("computeNumericalGradient.R")
#checkCostFunction()

##############  Cost Regularization

J <- cofiCostFunc(Y, R, num_users, num_movies, 
num_features, 1.5)(c(c(X),c(Theta)))

##############  Gradient Regularization

#checkCostFunction(1.5)

##########adding my ratings

movieList <- loadMovieList()
my_ratings <- rep(0,1682)

my_ratings[1] <- 4
my_ratings[98] <- 2
my_ratings[7] <- 3
my_ratings[12]<- 5
my_ratings[54] <- 4
my_ratings[64]<- 5
my_ratings[66]<- 3
my_ratings[69] <- 5
my_ratings[183] <- 4
my_ratings[226] <- 5
my_ratings[355]<- 5

########## Learning movie ratings
X=data2$X
Theta=data2$Theta
num_users=data2$num_users
num_movies=data2$num_movies
num_features=data2$num_features

#add my ratings

Y <- cbind(my_ratings, Y)
R <- cbind((my_ratings != 0), R)

source("normalizeRatings.R")
#normalize
NR  <- normalizeRatings(Y, R)
Ynorm <- NR$Ynorm
Ymean <- NR$Ymean
#  Useful Values
num_users <- dim(Y)[2]
num_movies <- dim(Y)[1]
num_features <- 10

#Initparam
n <- num_movies * num_features
X <- matrix(rnorm(n), num_movies, num_features)

n <- num_users * num_features
Theta <-  matrix(rnorm(n), num_users, num_features)

initial_parameters <- c(c(X), c(Theta))

#Regularization
lambda <- 10

cF <- cofiCostFunc(Y, R, num_users, num_movies,num_features, lambda)
gF <- cofiGradFunc(Y, R, num_users, num_movies,num_features, lambda)

theta <- optim(initial_parameters, fn = cF, gr = gF,
       method = "BFGS", control = list(maxit=10, trace=1, REPORT=1) )$par

# unfold
X <- matrix(theta[1:(num_movies*num_features)], num_movies, num_features)
Theta <- matrix(theta[(num_movies*num_features+1):length(theta)], 
num_users, num_features)



