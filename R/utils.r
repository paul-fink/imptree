# this file contains internal utility functions, which are used by more than
# one function call and are too small to justify a separat file

# calculation of the t-value
calcT <- function(maxE, minE, maxEbase, minEbase, maxEposs, gamma) {
  if(maxE < maxEbase) {
    pessimism <- (maxEposs - maxEbase) / (maxEposs - maxE)
    optimism <- (minEbase - minE) / (maxE + abs(minE - minEbase))
    T <- gamma * pessimism - (1 - gamma) * optimism
    if(maxE < minEbase) {
# T may get at minimum -1 so with -3 we are guaranteed to have it as splitting candidate
      T <- T - 3
    }
  } else {
# just to make sure we never split
# the value it is compared to is always in [0,1]
    T <- 2
  }
  T
}
