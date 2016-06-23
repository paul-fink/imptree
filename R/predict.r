#
# This function carries out the prediction of an imprecise tree.
#
# Input:
#   object:    An object of class 'imptree'
#   newdata:   Data.frame containing observations to be predicted. If NULL the observations
#                in the training set of 'object' are employed. Any observation with a NA value
#                in any splitting variable will be omitted
#   type:      Type of prediction. This may be either 'class' (Default) for classes
#                or 'prob' for probability intervals.
#   dominance: Dominace criterion to be applied when predictin classes. This may either
#                be 'strong' (Default) or 'max'.
#                For furhter details see function 'predclass.'
#   ...:       Required for compatibility in calls of 'accuracy'.
#
# Output:
#   Erg:  In case of 'class': A boolean matrix is returned, with a row for each observation.
#                               A TRUE value means that the class is predicted.
#         In case of 'prob':  A list which contains for each observation a list, giving the
#                               upper and lower bounds of the probability intervals.
#

predict.imptree <- function(object, newdata = NULL, type = c("class", "prob"), dominance = c("strong", "max"), ...) {

# only objects of class 'imptree' are valid (Should be not a problem when calling from 'predict')
  if(!inherits(object, "imptree")) {
    stop("object not of class 'imptree'")
  }
# if no data supplied,the ones in object are taken
  if(is.null(newdata)) {
    warning("Predicting values of the generation data set")
    newdata <- object$Data
  }
# partial matching of the prediction type
  type <- match.arg(type)
# reducing the data.frame to just those feature variables used in splitting
  new.form <- if(is.null(object$Splitnames)) {
    object$formula
  } else {
    as.formula(paste(".~",paste(object$Splitnames, collapse = "+")))
  }
# creating the data according to the formula
  Dt <- model.frame(delete.response(terms(new.form)), na.action = na.omit, newdata)
# extracting the levels for each factor variable in the data set
  fact <- sapply(Dt, function(x) !is.null(levels(x)))
  char <- sapply(Dt, is.character)
  sDt <- 1:ncol(Dt)
  if (any(fact | char)) {
    for (j in sDt[char]) Dt[[j]] <- as.factor(Dt[[j]])
    fact <- fact | char
    cat.levels <- lapply(Dt[fact], levels)
    for (j in sDt[fact]) Dt[[j]] <- as.integer(Dt[[j]])
    attr(Dt, "cat.levels") <- cat.levels
  }
# data.frame of observations into list of observations
  ldata <- lapply(seq(along = rownames(Dt)), function(x) {y <- data.frame(as.matrix(Dt[x,], nrow = 1)); rownames(y) <- 1; y})

# initializing the result  
  Erg <- vector()

# getting out the levels for each feature variable
  cat.levels <- attr(Dt, "cat.levels")
# calling the function "pred_imptree" which does the actual classification by climbing down the tree
  Erg <- .Call("pred_imptree", ldata, object$Tree, names(Dt), cat.levels)
# when the classes are to be predict, the list of observation is concerted back into a matrix
  if(type == "class") {
# partial matching of the prediction method when predicting classes
    dominance <- match.arg(dominance)
    Erg <- data.frame(t(sapply(Erg, function(x) {predclass(lower = x$Lower, upper = x$Upper, method = dominance)})), check.names = FALSE)

  }
  Erg <- list(predicted = Erg)
  Erg
}


#
# This function carries out the prediction of a bag of imprecise trees.
#
# Input:
#   object:      An object of class 'impbag'
#   newdata:     Data.frame containing observations to be predicted. If NULL the observations
#                  in the training set of 'object' are employed. Any observation with a NA value
#                  in any splitting variable will be omitted
#   type:        Type of prediction. This may be either 'class' (Default) for classes
#                  or 'prob' for probability intervals.
#   dominance:   Dominace criterion to be applied when predictin classes. This may either
#                  be 'strong' (Default) or 'max'.
#                  For furhter details see function 'predclass.'
#   aggregation: Aggregation method to be applied:
#                  'no': No aggregation is carried out (Default).
#                  'equal': Majoirty voting is applied.
#                  'dacc': Trees are weighted according to their attained discounted-accuracy
#                            and then majority voting is applied over aggregate.
#                  'disjunction':  The disjunction rule is applied.
#                  'mean': the average rule is applied.             
#   ...:           Additional parameters may be passed, but not used yet.
#
# Output:
#   A named list containing the following elements
#     -predicted: The prediction of the classes in a data.frame. Special cases:
#                   -'no': a list containing the prediction for each tree
#     -probs:     The prediction of the probability intervals. Special cases:
#                   -'equal' and 'dacc': NULL
#                   -'no': a list containing the prediction for each tree
#

predict.impbag <- function(object, newdata = NULL, dominance = c("strong", "max"),
                    aggregation = c("no", "equal", "dacc", "disjunction", "mean"), ...) {

# only objects of class 'impbag' are valid (Should be not a problem when calling from 'predict')
  if(!inherits(object, "impbag")) {
    stop("object not of class 'impbag'")
  }
# if no data supplied,the ones in object are taken
  if(is.null(newdata)) {
    warning("Predicting values of the generation data set")
    newdata <- object$Data
  }
# partial matching of the prediction method
  dominance <- match.arg(dominance)
# partial matching the aggregation method
  agm <- match.arg(aggregation)
# removing all observations with any NA in a splitting variable used in the supplied formula
  newdata <- model.frame(object$formula, data = newdata, na.action = na.omit)
# getting the bag
  bag <- object$Bag
# initializing the ouput values
  probInt <- predicted <- NULL
  if(agm %in% c("no", "equal", "dacc")) {
# dealing with class predictions
# function to apply accuracy calculation to a tree
    pred.fun <- function(imptree) {
      pred.tree <- accuracy(imptree, newdata, dominance = dominance)
      pred.tree
    }
# applying the calculation of accuracy to each tree
    erg <- lapply(bag, FUN = pred.fun)
# collecting all results (extracting out of 'accuracy' and 'predict.imptree' result)
    Erg <- lapply(erg, function(x) {x$prediction$predicted})
# Should aggregation take place?
    if(agm != "no") {
# the weights are calculated 
# weights are eitehr 1 or the discounted-accuracy of a tree
      weights <- if(agm == "dacc") {
        lapply(erg, function(x) {x$acc$discountedacc})
      } else {
        as.list(rep(1, length(erg)))      
      }
# weights are multplied on the resulting matrices
      Erg <- lapply(seq_along(Erg), function(i) {Erg[[i]] * weights[[i]]})
# summing over all matrices
      Erg <- Reduce("+", Erg)
# identifying the maximum for each row by TRUE, all others FALSE 
      Erg <- data.frame((Erg == apply(Erg, 1, FUN = max)), check.names = FALSE)
    }
# assigning the result to the final structue to be returned
    predicted <- Erg
  }
  if(agm %in% c("no", "disjunction", "mean")) {
# dealing with probability interval based predictions
# function to predict a tree
    pred.fun <- function(imptree) {
      pred.tree <- predict(imptree, newdata, type = "prob", dominance = dominance)
      pred.tree
    }
# applying the predciton to each tree
    erg <- lapply(bag, FUN = pred.fun)
# Should aggregation be carried out?
    if(agm != "no") {
# number of observations
      n <- NROW(newdata)
# rownames of the data
      rn <- row.names(newdata)
# reodering the strucure (list(trees = list(observations))  -> list(observations = list(trees)) 
      ores <- lapply(seq_len(n), function(k) {
        h1res <- lapply(seq_along(erg), function(kk){
          h2res <- erg[[kk]][[1]][[k]]
          h2res
        })
        h1res
      })
# for each observation the aggregation is performed      
      tres <- lapply(ores, function(ore) {
        if(agm == "disjunction") {
# sapply creates matrix where the row contain the class levels and the columns are the trees
# then with apply the minimum of the lower and the maximum of the upper bounds is obatained row-wise
          u <- apply(sapply(ore, function(x) x$Upper), 1, max)
          l <- apply(sapply(ore, function(x) x$Lower), 1, min)
        } else {
# sapply creates matrix where the row contain the class levels and the columns are the trees
# then the mean is obtained row-wise for both upper and Lower
          u <- rowMeans(sapply(ore, function(x) x$Upper))
          l <- rowMeans(sapply(ore, function(x) x$Lower))
        }
# finally predicting the final class(es) for the observation
        p <- predclass(upper = u, lower = l, method = dominance)
# storing all information in a list
        list(Probs = list(Lower = l, Upper = u), Predicted.Class = p)
      })
# extracting the predicted classes and putting them into a data.frame
      predicted <- data.frame(t(sapply(tres, function(x) x$Predicted.Class)), check.names = FALSE, row.names = rn)
# extracting the predicted probability intervals
      probInt <- lapply(tres, function(x) x$Probs)
    } else {
# when no aggregation is performed only the raw list of proability intervals is returned
# which is still in the order of list(trees = list(observations))
      probInt <- erg
    }
  }
# combining them into the final output structure
  Erg <- list(predicted = predicted, probs = probInt)
# calling object again to be returned
  Erg
}

