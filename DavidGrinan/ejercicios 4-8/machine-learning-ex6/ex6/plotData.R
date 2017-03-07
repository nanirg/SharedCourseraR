#plot function


plotData <- function (X, y) {

  symbolss <- c(21,3) #plus and fillded circle
  yfac <- factor(y)
  plot(X[,1],X[,2], pch = symbolss[yfac] ,bg = "yellow", lwd = 1.3)
  
}