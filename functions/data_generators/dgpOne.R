# DGP #1 for simulations of optimal weighting thing
# Nik Julius

dgpOne <- function(numTreated, numControls, trueTau) {
  
  # Y = x + wt + e
  # x ~ N(2,5)
  # e ~ N(0, sig_i^2)
  
  numObs <- numTreated + numControls
  
  # sig_i^2 is drawn from U[0.5,1.5]
  
  sigmas <- runif(numObs, 0.5, 1.5)
  
  # draw errors
  
  e <- rep(NA, times = numObs)
  
  for(i in 1:length(e)) {
    e[i] <- rnorm(1, 0, sigmas[i])
  }
  
  # draw covariates
  
  x <- rnorm(numObs, 2, 5)
  
  # Randomly assign treatment
  
  treatedIndices <- sample(seq(from = 1, to = numObs, by = 1), size = numTreated, replace = FALSE)
  
  w <- rep(0, times = numObs)
  
  w[treatedIndices] <- 1
  
  # Generate outcomes
  
  Y <- x + w*trueTau + e
  
  Z <- cbind(Y, x, w)
  
  return(Z)
  
  
}
