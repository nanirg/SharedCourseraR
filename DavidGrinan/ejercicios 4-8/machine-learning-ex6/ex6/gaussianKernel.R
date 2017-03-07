gaussianKernel <- function(sigma) {
 
  function(x1, x2) {
    
    x1 <- c(x1)
    x2 <- c(x2)
    
    # You need to return the following variables correctly.
    sim <- 0
    
    # ----------------------- YOUR CODE HERE -----------------------
    # Instructions: Fill in this function to return the similarity between x1
    #               and x2 computed using a Gaussian kernel with bandwidth
    #               sigma
    #
    #
    sim <- exp(-(sum((x1 - x2) ^ 2)) / (2 * sigma ^ 2))
    sim
  }
  # --------------------------------------------------------------
  
}
