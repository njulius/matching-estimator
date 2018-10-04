# Matching Estimator Implementation
# Niklaus Julius

genEstimators <- function(augTreatedArm, augControlArm) {
  
  n1 <- length(augTreatedArm[,1])
  n0 <- length(augControlArm[,1])
  n <- n1 + n0
  
  # First, calculate ATET
  # Mean of treated outcomes minus weighted mean of control outcomes
  atet <- mean(augTreatedArm[,1]) - mean(augControlArm[,1]*augControlArm[,7])
  
  # Then calculate ATEU
  # Mean of weighted treated outcomes minus mean of control outcomes
  ateu <- mean(augTreatedArm[,1]*augTreatedArm[,7]) - mean(augControlArm[,1])
  
  # Calculate ATE
  # Weighted mean on both sides
  ate <- (1/n)*sum(augTreatedArm[,1]*(1 + augTreatedArm[,7]) - augControlArm[,1]*(1 + augControlArm[,7]))
  
  # Sanity Checker
  # Calculate ATE as weighted average of ATET and ATEU
  sanity <- (n1/n) * atet + (n0/n) * ateu
  
  results <- c(atet, ateu, ate, sanity)
  
  return(results)
  
}
