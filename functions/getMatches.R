# Matching Estimator Implementation
# Nik Julius

getMatchesTC <- function(treatedArm, controlArm, nMatches) {
  
  require(Matrix)
  
  # Function takes a sample that has already been split by treatment into separate arms
  # and returns a sparse logical matrix that indicates which units are matched to what.
  
  # The matrix will look like:
  # # # # # # # # # # #  
  #     T1  T2  T3  T4# Where an entry of '1' indicates that the control unit in that row 
  # C1  1   -   -   - # is matched to the treatment unit in that column. Thus, in this
  # C2  -   1   1   - # example, C1 is closest to T1, C2 is closest to T2 and to T3, C3 is
  # C3  -   -   -   - # closest to none of the treated units, and C4 is closest to T4.
  # C4  -   -   -   1 #
  # # # # # # # # # # #
  
  # Create matrix with the same row/column structure as the above, where each cell contains
  # the absolute value of the distance between the covariate value of the relevant treated
  # and control units
  
  # Not that this assumes the covariate vector is the second column in the split sample.
  
  TCdistMatrix <- apply(treatedArm, 1, function(x) abs(x[2] - controlArm[,2]))
  
  # Now, since we are potentially finding more than the single nearest neighbor, we need to
  # determine the 'caliper' that is implied for each observation. If, say, we are matching
  # the three nearest neighbors, then the 'caliper' is the distance between the target and
  # the 3rd lowest value in the target's column of distMatrix.
  
  # So, in contrast to the earlier version, instead of testing whether each cell in distMatrix
  # is equal to the min over that column, we tes twhether it is less than or equal to the 
  # nMatches'th entry in sort(distMatrix[,i]).
  
  TCmatches <- Matrix(apply(TCdistMatrix, 2, function(x) x <= sort(x)[nMatches]), sparse = TRUE)
  
  return(TCmatches)
  
}

# Identical function to getMatchesTC but it inverts the row/column structure above, so that it
# finds nMatches treated units to match for each control unit.

getMatchesCT <- function(treatedArm, controlArm, nMatches) {
  
  require(Matrix)
  
  CTdistMatrix <- apply(controlArm, 1, function(x) abs(x[2] - treatedArm[,2]))
  
  CTmatches <- Matrix(apply(CTdistMatrix, 2, function(x) x <= sort(x)[nMatches]), sparse = TRUE)
  
  return(CTmatches)
  
}
