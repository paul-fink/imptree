# wrapper function for the maxEntropy distribution calculation for IDM
maxEntropyIDM <- function(upper, lower) {

  maxEnt <- .C("maxEntropyIDM", lower = as.numeric(lower), upper = as.numeric(upper), dim = as.integer(length(lower)))
  maxEnt$lower

}

# wrapper function for the minEntropy distribution calculation for IDM
minEntropyIDM <- function(upper, lower) {

  minEnt <- .C("minEntropyIDM", lower = as.numeric(lower), upper = as.numeric(upper), dim = as.integer(length(lower)))
  minEnt$lower

}

# calculation of the entropy based on the supplied distribution
# BE CAREFUL: NO CONSISTENCY CHECKS ON THE PROPERTIES OF THE DISTRIBUTION
calcEntropyIDM <- function(distribution, correction = c("no", "abellan", "strobl"), s = NA, N = NA) {

  correction <- match.arg(correction)
# making sure that a 0 entry remains after applying the entropy function
  ent <- (-1) * sum(sapply(distribution, function(x) {if(x > 0) x * log(x, base = 2) else 0}))
# only proceed into the correction if there is consistent information available of 'N' and 's'
  if(!is.na(s) && s >= 0 && !is.na(N) && N >= 0 && correction != "no") {
    if(correction == "abellan") {
# the Abellan/Moral approach of adding an Hartley measure to the entropy
      ent <- ent + (s * log(length(distribution), base = 2)) / (N + s)
    } else if(correction == "strobl") {
# the Strobl approach by correcting it with a Miller-Entropy based correction to account for different
# numbers of categories in the splitting candidates
      ent <- ent + ((length(distribution) + 1) / (2 * N + s))
    } else {
# we are left cluesless here, yet with the 'match.arg' we should never encounter it as a warning
      warning(paste("Unrecognized correction method: '", correction, "'.\nApplying no correction!"), sep = "")
    }
  } else if(correction != "no") {
# a correction method was specified, but no consistent parameters were supplied thus no correction
    warning("No valid values 's' and 'N' supplied for correction method.\nApplying no correction!")
  }
  ent
}

# calculation of the T-values in case of the IDM
getTValuesIDM <- function(NAMES, x, y, s, correction, gamma) {

# frequencies of the classification variable
  fry <- table(y)
# denominator for the results of the IDM
  N <- sum(fry)
  denom <- N + s
# calculation of the upper and lower bounds according to the IDM
  upper <- (fry + s) / denom
  lower <- fry / denom
# calculation of the maximum/minimum entropy distribution along with the maximal possible entropy
  maxEntBase <- calcEntropyIDM(maxEntropyIDM(upper = upper, lower = lower), correction = correction, s = s, N = N)
  minEntBase <- calcEntropyIDM(minEntropyIDM(upper = upper, lower = lower), correction = correction, s = s, N = N)
  maxEntPoss <- calcEntropyIDM(rep(1 / dim(fry), dim(fry)), correction = correction, s = s, N = N)
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
      hdenom <- sum(hfrey) + s
      hupper <- (hfrey + s) / hdenom
      hlower <- hfrey / hdenom
      maxEnt[i] <- calcEntropyIDM(maxEntropyIDM(upper = hupper, lower = hlower), correction = correction, s = s, N = sum(hfrey))
      minEnt[i] <- calcEntropyIDM(minEntropyIDM(upper = hupper, lower = hlower), correction = correction, s = s, N = sum(hfrey))
    }
    if(sum(table(x[, nm])) > 0) {
# obtaining the upper/lower entropy for a splitting by weighted sum of those in the daughter nodes
      upperEnt <- sum(table(x[, nm])/sum(table(x[, nm])) * maxEnt)
      lowerEnt <- sum(table(x[, nm])/sum(table(x[, nm])) * minEnt)
# computing the T-value
      tvalue[nm] <- calcT(maxE = upperEnt, minE = lowerEnt, maxEbase = maxEntBase, minEbase = minEntBase, maxEposs = maxEntPoss, gamma = gamma)
    } else {
# set T-Value high, so we never split
      tvalue[nm] <- Inf
    }
  }
# return the array of T-values including the calculated upper and lower entropy
  list(tvalue = tvalue, upper = upper, lower = lower)
}
