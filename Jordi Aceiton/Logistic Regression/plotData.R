plotData <- function(X, y, xy_labels= character(2), legend_labels= character(2)) {

## =============================================================================
## plotData(x,y) plots the data points with + for the positive examples
##               and o for the negative examples. X is assumed to be a Mx2 matrix.
## =============================================================================
                
        plot(x=X[,2], y=X[,3], 
             type = "n",
             xlab = xy_labels[1],
             ylab = xy_labels[2])
        
        points(x=X[,2][y==1], y=X[,3][y==1], pch=3, col="black" )
        points(x=X[,2][y==0], y=X[,3][y==0], pch=19, col="yellow" )
        legend("topright", legend = legend_labels, 
               cex = 0.7, 
               pch = c(3,19), 
               col= c("black", "yellow"))
}