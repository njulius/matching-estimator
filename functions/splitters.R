# Matching Estimator Implementation
# Nik Julius

treatedSplit <- function(Z) {
  
  # Take incoming dataset Z and column numbers for the outcome, covariate, and indicator
  # columns. Split dataset into treated and control arms, arrange columns appropriately
  # for later use, and return the treated arm. Control arm is discarded because controlSplit()
  # will do the same to generate the control arm.
  
  treatedArm <- Z[which(Z[,3] == 1),]
  
  return(treatedArm)
  
}

controlSplit <- function(Z) { 
  
  controlArm <- Z[which(Z[,3] == 0),]
  
  return(controlArm)
  
}
