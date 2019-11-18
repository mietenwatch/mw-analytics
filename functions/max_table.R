#function to find most frequent categorial value, exclude nullvalue
max_table <- function(InVec, mult = FALSE, excl="keine Informationen") {
  InVec <- InVec [! InVec %in% excl]
  if (!is.factor(InVec)) InVec <- factor(InVec)
  A <- tabulate(InVec)
  if (isTRUE(mult)) {
    levels(InVec)[A == max(A)]
  }
  else levels(InVec)[which.max(A)]
} 