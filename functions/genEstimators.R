# Matching Estimator Implementation
# Niklaus Julius

getEstimates <- function(augTreatedArm, augControlArm) {
  
  n1 <- length(augTreatedArm[,1])
  n0 <- length(augControlArm[,1])
  n <- n1 + n0
  
  # Generate ATET
  
  atet <- (sum(augTreatedArm[,1]) - sum(augControlArm[,1]*augControlArm[,7]))*(1/n1)
  
  # Generate ATEU
  
  ateu <- (sum(augTreatedArm[,1]*augTreatedArm[,7]) - sum(augControlArm[,1]))*(1/n0)
           
  # Generate ATE
  
  treatedIntermediate <- sum(augTreatedArm[,1]) + sum(augTreatedArm[,1]*augTreatedArm[,7])
  controlIntermediate <- sum(augControlArm[,1]) + sum(augControlArm[,1]*augControlArm[,7])
  
  ate <- (1/n)*(treatedIntermediate - controlIntermediate)
  
  # Generate Sanity
  
  sanity <- (n1/n)*atet + (n0/n)*ateu
  
  results <- cbind(atet, ateu, ate, sanity)
  
  return(results)
  
}
