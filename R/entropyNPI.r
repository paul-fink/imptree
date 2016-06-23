# wrapper function for the maximum entropy distribution calculation for the NPI
maxEntropyNPI <- function(observations, method = c("approx", "exact")) {
  
  method <- match.arg(method);
  dim <- as.integer(length(observations))
  if(method == "exact") {
    maxEnt <- .C("maxEntropyNPIe", obs = as.integer(observations), dim = as.integer(length(observations)), prob = numeric(dim))
  } else {
    maxEnt <- .C("maxEntropyNPIa", obs = as.integer(observations), dim = as.integer(length(observations)), prob = numeric(dim))
  }
  maxEnt$prob

}

# wrapper function for the minimum entropy distribution calculation for the NPI
minEntropyNPI <- function(observations) {

  dim = as.integer(length(observations))
  minEnt <- .C("minEntropyNPIa", obs = as.integer(observations), dim = as.integer(length(observations)), prob = numeric(dim))
  minEnt$prob

}

# calculation of the entropy based on the supplied distribution
# BE CAREFUL: NO CONSISTENCY CHECKS ON THE PROPERTIES OF THE DISTRIBUTION
calcEntropyNPI <- function(distribution, correction = c("no", "strobl"), N = NA) {

  correction <- match.arg(correction)
# making sure that a 0 entry remains after applying the entropy function
  ent <- (-1) * sum(sapply(distribution, function(x) {if(x > 0) x * log(x) else 0}))
# only proceed into the correction if there is consistent information available of 'N'
  if(!is.na(N) && N >= 0 && correction != "no") {
    if(correction == "strobl") {
# the Strobl approach by correcting it with a Miller-Entropy based correction to account for different
# numbers of categories in the splitting candidates as adapted to NPI by Baker
      ent <- ent + ((length(distribution) - 1) / (2 * N))
    } else {
# we are left cluesless here, yet with the 'match.arg' we should never encounter it as a warning
      warning(paste("Unrecognized correction method: '", correction, "'.\nApplying no correction!"), sep = "")
    }
  } else if(correction != "no") {
# a correction method was specified, but no consistent parameters were supplied thus no correction
    warning("No valid value 'N' supplied for correction method.\nApplying no correction!")
  }
  ent
}

# calculation of the T-values in case of the NPI
getTValuesNPI <- function(NAMES, x, y, maxEntropyMethod, correction, gamma) {

# frequencies of the classification variable
  fry <- table(y)
  N <- sum(fry)
# calculation of the upper and lower probability bounds
  upper <- sapply((fry + 1) / N, min, 1)
  lower <- sapply((fry - 1) / N, max, 0)
# calculation of the maximum/minimum entropy distribution along with the maximal possible entropy
  maxEntBase <- calcEntropyNPI(maxEntropyNPI(observations = fry, method = maxEntropyMethod), correction = correction, N = N)
  minEntBase <- calcEntropyNPI(minEntropyNPI(observations = fry), correction = correction, N = N)
  maxEntPoss <- calcEntropyNPI(rep(1 / dim(fry), dim(fry)), correction = correction, N = N)
# initializing an array storing all T-values for the different splitting variables under consideration
  tvalue <- rep(0, length(NAMES))
  names(tvalue) <- NAMES
# filling the array with T-values
  for (nm in NAMES) {
# splitting the classification variable according to the splitting variable under consideration (svuc)
    l <- split(y, x[, nm])
# initialising an array of entropy values for each possible node of svuc
    m <- length(l)
    maxEnt <- rep(0, m)
    minEnt <- rep(1, m)
# iteration over all possible nodes of svuc
    for(i in seq_along(l)){
# observations of class variable in daughter node
      ll <- l[[i]]
# calculating the upper entropy in the daughter (same scheme as in mother node)
      hfrey <- table(ll)
      maxEnt[i] <- calcEntropyNPI(maxEntropyNPI(observations = hfrey, method = maxEntropyMethod), correction = correction, N = sum(hfrey))
      minEnt[i] <- calcEntropyNPI(minEntropyNPI(observations = hfrey), correction = correction, N = sum(hfrey))
    }
# obtaining the upper/lower entropy for a splitting by weighted sum of those in the daughter nodes
    upperEnt <- sum(table(x[, nm])/sum(table(x[, nm])) * maxEnt)
    lowerEnt <- sum(table(x[, nm])/sum(table(x[, nm])) * minEnt)
# computing the T-value
    tvalue[nm] <- calcT(maxE = upperEnt, minE = lowerEnt, maxEbase = maxEntBase, minEbase = minEntBase, maxEposs = maxEntPoss, gamma = gamma)
  }
# return the array of T-values including the calculated upper and lower entropy
  list(tvalue = tvalue, upper = upper, lower = lower)
}
