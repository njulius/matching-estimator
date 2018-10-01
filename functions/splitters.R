# Matching Estimator Implementation
# Nik Julius

treatedSplit <- function(Z, outcome, covariate, indicator) {
  
  # Take incoming dataset Z and column numbers for the outcome, covariate, and indicator
  # columns. Split dataset into treated and control arms, arrange columns appropriately
  # for later use, and return the treated arm. Control arm is discarded because controlSplit()
  # will do the same to generate the control arm.
  
  Y <- Z[,outcome]
  X <- Z[,covariate]
  W <- Z[,indicator]
  
  cont <- cbind(Y,X,W)
  
  treatedArm <- cont[which(cont[,3] == 1),]
  
  return(treatedArm)
  
}

controlSplit <- function(Z, outcome, covariate, indicator) { 
  
  Y <- Z[,outcome]
  X <- Z[,covariate]
  W <- Z[,indicator]
  
  cont <- cbind(Y,X,W)
  
  controlArm <- cont[which(cont[,3] == 0),]
  
  return(controlArm)
  
}
