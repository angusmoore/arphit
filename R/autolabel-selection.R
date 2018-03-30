distancefitness <- function(candidate, series.x, data, serieslist) {
  cost <- 0
  for (label in names(candidate)) {
    a <- candidate[[label]][1]
    b <- candidate[[label]][2]
    losdistance <- series.distance(a, b, series.x, data, serieslist, label, TRUE)$distance
    nolosdistance <- series.distance(a, b, series.x, data, serieslist, label, FALSE)$distance
    cost <- cost + min(losdistance, nolosdistance + LOSPENALTY)
  }
  return(cost)
}

bestcandidate <- function(candidates, series.x, data, labelsmap) {
  ncandidates <- length(candidates)

  # Check which has the most labels
  nlabels <- c()
  for (candidate in candidates) {
    nlabels <- append(nlabels, length(candidate))
  }
  lmax <- max(nlabels)
  if (sum(nlabels == lmax) == 1) {
    # Is a unique maximum
    return(candidates[nlabels == lmax][[1]])
  } else {
    # Keep those candidates that are tied, drop others
    candidates <- candidates[nlabels == lmax]
  }

  # Fitness by distance (with penalty for no LOS)
  distances <- c()
  for (candidate in candidates) {
    distances <- append(distances, distancefitness(candidate, series.x, data, names(labelsmap)))
  }
  dmin <- min(distances)
  if (sum(distances == dmin) == 1) {
    # Is a unique maximum
    return(candidates[distances == dmin][[1]])
  } else {
    # Just return the first one of those with a tie
    return(candidates[distances == dmin][[1]])
  }
}
