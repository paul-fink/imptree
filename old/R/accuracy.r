#######################################################################################
# This function calculates the accuracy based on a tree structure and a data set.     #
# In case no data set is supplied the accuracy of the training data is assessed.      #
# As its applicable for both object of class 'impbag' and 'imptree', an aggregation   #
# method may be supplied alongwith, which is ignored for objects of class 'imptree'.  #
#                                                                                     #
# Input:                                                                              #
#   object:      An object of class 'imptree' or 'impbag'                             #
#   data:        data.frame on which the accuracy should be assessed.                 #
#                  When not supplied the training data of 'object' are used           #
#   aggmethod:   Which aggregation method should be applied for a bag:                #
#                  'equal':  Majority voting rule (Default)                           #
#                  'dacc':   Weighted Majority voting rule based on attained          #
#                              discounted accuracy of the single trees                #
#                  'disjunction': Disjunction rule with Interval Dominance            #
#                  'mean': Average rule rule with Interval Dominance                  #
#                This parameter is ignored in case object is of class 'imptree'       #
#   dominance:   Dominance criterion to apply when predicting classes                 #
#                                                                                     #
# Output:                                                                             #
#   A named list of the following:                                                    #
#     - acc:  named list containing the achieved accuracy                             #
#       - determinacy:    Determinacy                                                 #
#       - singleacc:      Single-set accuracy (NA when no determinate observation)    #
#       - setacc:         set-accuracy (NA when no indeterminate observation)         #
#       - nindeterminant: Average number of classes when predicting indeterminate     #
#                           (NA when no indeterminate observation)                    #
#       - discountedacc:  Discounted-accuracy                                         #
#     - prediction: Object returned by call to 'predict'                              #
#                                                                                     #
#######################################################################################

accuracy <- function(object, data = NULL, aggmethod = c("equal", "dacc", "disjunction", "mean"), dominance = c("strong", "max")) {
# object of class 'imptree' or 'impbag' is required
  if(missing(object) || !(inherits(object, "imptree") || inherits(object, "impbag"))) {
    stop("object not of class 'imptree'")
  }
# mathing the prediction method
  dominance <- match.arg(dominance)
# if no extra data are supplied, then training data of 'object' are employed
  if(is.null(data)) data <- object$Data
  if(inherits(object, "imptree")) {
# to measure accuracy the class are required (ignoring aggregation methods as its a tree)
    p <- predict(object, newdata = data, type = "class", dominance = dominance)
  } else {
# partial matching to the aggregation methods
    aggm <- match.arg(aggmethod)
    p <- predict(object, newdata = data, aggregation = aggm, dominance = dominance)
  }
# getting out the class predictions
  pp <- p$predicted
# extracting the true class values from within the data
  y <- data[names(data) == object$formula[[2]]]
  n <- NROW(pp)
# getting a boolean vector with stores if true values are within predicted classes
  ts <- sapply(seq_len(n), function(i) {
    y[i, ] %in% names(pp[i, ])[unlist(pp[i, ])]
  })
# subset of prediction which contains only determinate
  ps <- pp[rowSums(pp)>1, , drop = FALSE]
# indicator if a determinate prediction exists at all
  det <- n-NROW(ps)
# determinacy
  deter <- if(det) mean(rowSums(pp) == 1) else 0
# single set accuracy (NA)
  siacc <- if(det) mean(ts[rowSums(pp) == 1]) else NA
# set accuracy
  seacc <- if(NROW(ps) > 0) mean(ts[rowSums(pp) > 1]) else NA
# number of average indeterminate classes
  ind <- if(NROW(ps) > 0) mean(rowSums(ps)) else NA
# discounted accuracy
  dacc <- mean(ts/rowSums(pp))
# combining the results
  result <- list(acc = list(determinacy = deter, singleacc = siacc, setacc = seacc, nindeterminant = ind, discountedacc = dacc), prediction = p)
  result
}
