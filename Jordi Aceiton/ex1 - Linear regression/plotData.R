plotData <- function(x, y) {
        
        par(mar = c(4,4,2,4))
       
         plot(x, y, 
             col = "red",
             pch = 4,
             xlab = "Population of city in 10,000s.",
             ylab = "Profit in $10,000s.",
             xlim = range(4:24),
             ylim = range(-5:25),
             xaxt = "n")
        
        axis(1, at = seq(4,24, by=2))
}