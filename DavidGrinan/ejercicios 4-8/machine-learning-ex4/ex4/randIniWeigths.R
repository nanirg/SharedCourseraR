randIniWeights <- function(L_in,L_out){

	W <- matrix(0,L_out, 1 + L_in)
	epsilon_init <- 0.11881
	rnd <- runif(L_out * (1 + L_in))
	rnd <- matrix(rnd,L_out,1 + L_in)
	W <- rnd * 2 * epsilon_init - epsilon_init
	randIniWeights = W


}