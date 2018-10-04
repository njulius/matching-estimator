# Matching Estimator Implementation
# Niklaus Julius

appendWeightsTC <- function(treatedArm, controlArm, numMatches) {
  
  # Do the matching
  
  matchesTC <- getMatchesTC(treatedArm, controlArm, numMatches)
  
  # Get the weights for each control unit
  
  k <- rowSums(matchesTC)
  
  # Append k to correct arm and return it
  
  augControlArm <- cbind(controlArm, k)
  
  return(augControlArm)
  
}

appendWeightsCT <- function(treatedArm, controlArm, numMatches) {
  
  matchesCT <- getMatchesCT(treatedArm, controlArm, numMatches)
  
  k <- rowSums(matchesCT)
  
  augTreatedArm <- cbind(treatedArm, k)
  
  return(augTreatedArm)
  
}
