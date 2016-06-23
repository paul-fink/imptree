#
# function to predict the final class(es) based on a dominance criterion
# Input:
#   lower:  vector of length N containing the lower bounds of the probability intervals
#   upper:  vector of length N containing the upper bounds of the probability intervals
#   method: Dominace criterion to be applied
#             'stong':  Interval Dominance criterion (Default)
#             'max':  Maximality criterion
#
# Output:
#   named boolean vector of length N: For any index it is 'TRUE'
#                                       when the class according to it is predicted
#

predclass <- function(lower, upper, method = c("strong", "max")) {
# partial matching of the method
  method <- match.arg(method)
# checking for proper input
  if(missing(upper) || missing(lower))
    stop("lower and upper need to be supplied")
  if(length(lower) != length(upper)) 
    stop("'lower' and 'upper' need to have same length")
# initializing the result with vacuous classification
  res <- rep(TRUE, length(upper))
  names(res) <- names(upper)
# in case of dominance criterion
  if(method == "strong") {
# for every upper bound it is checked if any lower bound is greater
# In case it happens then the result at the index of the upper is set to FALSE
    for(i in seq_along(res)) {
      u <- upper[i]
      if(any(rep(u, length(upper)-1) < lower[-i])) {
        res[i] <- FALSE
      }   
    }
# in case of maximum frequency
  } else if(method == "max"){
# just identify those with maximal upper bound
    res <- (upper == max(upper))
  } else {
# very unlikely this happens
    stop("unknown method for class prediction")
  }
  res
}
