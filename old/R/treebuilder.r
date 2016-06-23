# This functions grows an imprecise tree based on the the TreeBuild-algorithm 
# in Abellan and Moral: Upper entropy of credal sets
# Input:
#   formula:      Formula describing the strucutre (class variable ~ featutre variables)
#                   any interactions are ignored. Must be supplied!
#   data:         Data.frame used to evaluate to given formula on. If not provided the
#                   the formula is evaluated on the calling environment
#   testdata:     Data.frame to be employed as test set to measure accuracy. (Optional)
#   na.action:    What shall happen with 'NA's in the data (Default: 'na.pass')
#   control:      Named list controlling the growing and accuracy ouput
#                   Possible names are:
#                   - minbucket:    Minimal leaf size (Default: 1)
#                   - depth:        Positive integer limiting the tree to the given depth.
#                                     If not supplied tree is grown to maximal size (Default)
#                   - calc.acc:     Boolean; calculating accuracy on the 
#                                     training set (Default: FALSE)
#   T:            Value that needs to be at least attained to qualify for splitting
#   gamma:        Weighting factor of the pessimism
#   method:       Method to calculate the probability intervals (Default: IDM)
#                 Remark: NPI not yet implemented!!!
#   method.param: List containing the basic parameters of the supplied statistical model
#                 For IDM: s:          Parameter of the Imprecise Dirichlet Model (Default: 1)
#                          correction: Correction to be applied to the entropy calculation;
#                                      one of "no" (Default), "abellan", "strobl".
#                 For NPI: nothing yet
#   ...:          possibly futher arguments. Any arguments used in 'control' may be
#                   given here directly.     
#
# Output:
#   Object of class 'imptree' as a named list
#     - Call: The functions call
#     - Tree: The tree structure
#     - Data: The training data used to construct the tree
#     - formula: The formula describing the data structure
#     - Accuracy: Accuracy achieved on training data (NULL if calc.acc = FALSE)
#     - Test.result: Accuracy achieved on supplied test data (NULL if testdata = NULL)
#     - Splitnames: Names of the feature variables used for splitting
#
#
# In the following a tree structure is given. The upper case names are to be seen as variables
#
# PROBS = (Upper, Lower)
#   PROBS is a list containing the Upper and Lower bounds of the probability intervals
#   for the states of the classification variable
#     - Upper: Upper bounds
#     - Lower: Lower bounds
#
# NODEINFO = (Name, Obs, Splitnames, Probs = PROBS, Pred.class)
#   NODEINFO is a list containing basic information of the node:
#     - Name: Name of the Splitting variable (<None> when full tree is a single node)
#     - Obs:  Number of observations in the node
#     - Splitnames: Levels of the splitting variable (<None> when full tree is a single node)
#     - Probs: PROBS (s.o)
#     - Pred.class: Boolean vector giving the predicted states of the classification variable
#
# LEAF = (Name, Obs, Probs = PROBS, Pred.class)
#   LEAF is a list containing basic information of the leaf
#     - Name: <Leaf>
#     - Obs:  Number of observations in the node
#     - Probs: PROBS (s.o)
#     - Pred.class: Boolean vector giving the predicted states of the classification variable.
# KNOTS = (Splitpoit, Knot = (NODE | LEAF))
#   KNOTS is a list containing the splitpoint and the daughter node 
#     - Splitpoint: Value of the splitting variable assigned to the node
#     - Knot: Either a NODE or LEAF
#
# NODE = (Splits = NODEINFO, Knots = (KNOTS* | LEAF))
#   Node is a list consisting of basic information of the node and a list of daughter nodes
#     - Splits: SPLITINFO (s.o)
#     - Knots:  a list of KNOTS; or a LEAF (Only when full tree is a signle node!!)
#

imptree.formula <- function(formula, data = NULL, testdata = NULL, na.action = na.pass, 
                    control, gamma = 1, Tbase = 1, method = c("IDM", "NPI"),
                    method.param, ...) {

# function which performs the splitting within each node  
  treebuild <- function(x, minbucket, method, depth, gamma, Tbase, method.param) {

# classification variable
    y <- x[, 1]
# storing the complete data set with feature and class variables
    xold <- x
# feature variables
    x <- x[, -1]
# number of observations
    n <- NROW(x)
# possible splitting candidates (calculated according to a minimal node size)
    NAMES <- colnames(x)[sapply(x, function(z) all(table(z) >= minbucket))]
# number of levels of the class variable
    k <- nlevels(y)
# claculating the T-values for all the splitting variables
    if(method == "IDM") {
      result <- getTValuesIDM(NAMES = NAMES, x = x, y = y, s = method.param$s, correction = method.param$correction, gamma = gamma)
    } else {
      result <- getTValuesNPI(NAMES = NAMES, x = x, y = y, maxEntropyMethod = method.param$maxEntropyMethod, correction = method.param$correction, gamma = gamma)
    }
# exctracting the tvalues and the upper and lower entropy of the mother node from the result
    tvalues <- result$tvalue
    upper <- result$upper
    lower <- result$lower
# only proceed into nodes when the value of T is less than the treshold one
# and maximal depth not reach and there are splitting variable candidates
    if(depth && length(NAMES) && min(tvalues, na.rm=TRUE) < Tbase) {
# extracting index for which minimum T-value is attained
      index <- which(tvalues == min(tvalues, na.rm=TRUE))
# in case there are more than 1 splitting variable with same minimal value then 1 is sampled,
# getting the index of the variable and their index
      iname <- which(names(xold) == (nam <- names(index[sample(seq(along = index), 1)])))
# adding the splitting variable to the global list of splitting variables
      splitv <- get("split.variable.set")
      splitv <- c(nam, splitv)
      assign("split.variable.set", splitv, inherits = TRUE)
# splitting the full data without the splitting variable according to it
      dlist <- split(xold[, -iname], xold[, iname])
# obtaining the names of the splitting variable
      splits <- names(dlist)
# decreasing the depeth
      new.depth <- depth - 1
# initialisation of knot-list
      resultlist <- list()
# iteration over all knots
      for (j in seq_along(dlist)) {
# call this function again for every generated knot (keeping track of which value of the splitting variable was used)
        resultlist[[j]] <- list(Splitpoint = as.vector(splits[j]), Knot = treebuild(x = dlist[[j]], minbucket = minbucket, method = method,
				depth = new.depth, gamma = gamma, Tbase = Tbase, method.param = method.param))
      }
# return the tree structure
      return(list(Splits = list(Name = nam, Obs = n, Splitnames = splits, Probs = list(Upper = c(upper), Lower = c(lower))), Knots = resultlist))
    } else {
# in case we are already at maximum depth or no slitting candidate available
      splitv <- get("split.variable.set")
# basic structure of a leaf
      rknot <- list(Name = "<leaf>", Obs = n, Probs = list(Upper = c(upper), Lower = c(lower)))
      if(is.null(splitv)) {
# we are at root node an no splitting was performed, the node does not contain a knotlist, but a single knot (it self)
        return(list(Splits = list(Name = "<None>", Obs = n, Splitnames = "<None>", Probs = list(Upper = c(upper), Lower = c(lower))), Knots = rknot))
      } else {
# returnin knot structure 
        return(rknot)
      }
    }
  }
# end of recursive tree-growing function

# making sure a valid formula is supplied  
  if(missing(formula) || !inherits(formula, "formula")) {
    stop("Please supply a valid formula")
  } else  {
# constructing a data.frame according to the supplied formula and na.action    
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "na.action"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    if(is.null(mf$na.action)) mf$na.action <- na.action
    mf[[1L]] <- as.name("model.frame")
    x <- eval(mf, parent.frame())
  }
# checking for the calculation method
  method <- match.arg(method)
# initializing the control arguements such as minbucket, depth and calc.acc, they may also be supplied via '...'
  if (missing(control)) {
    control <- NULL
  }
  controls <- imptree_control(controlList = control, Tbase = Tbase, gamma = gamma, ...)
# all variables supplied through the formlua must be factors
  if(!all(sapply(x, is.factor))) stop("All used variables must be factors")
# checking for the method specific parameters
  method.param <- (get(paste("checkParam", method, sep = ".")))(method.param)
# saving the call
  call <- match.call()
# changing back the call to 'imptree'
  call[[1]] <- as.name("imptree")
# initializing global set of split variable
  split.variable.set <- NULL
# growing the tree
  tres <- treebuild(x = x, minbucket = controls$minbucket, method = method, depth = controls$depth, method.param = method.param, gamma = gamma, Tbase = T)
# getting the global set of used splitting variables
  split.variable.set <- unique(split.variable.set)
# construction of the structure of object 'imptree'
  res <- list(Call = call, Tree = tres, Data = x, formula = formula(terms(formula, data = x)), Accuracy = NULL, Test.result = NULL, Splitnames = split.variable.set)
# assigning class
  class(res) <- c("imptree")
# calculating accuracy if specified
  if(controls$calc.acc) res$Accuracy <- accuracy(res, dominance = controls$dominance)
# assessing accuracy on test set if supplied
  if(!is.null(testdata)) {
# reducing the data.frame to just those feature variables used in splitting
    if(!is.null(split.variable.set)) {
      testdata <- testdata[c(as.character(formula[[2]]), split.variable.set)]
    }
    res$Test.result <- accuracy(res, data = testdata, dominance = controls$dominance)
  }
  res
}

#
# Wrapping the imptree function for a non-formula case: y and x as arguments
#

imptree.default <- function(x, y, ...) {
  
  if(missing(y)|| missing(x)) {
    stop("Both y and x need to be supplied")
  }
  
  if(is.null(nam <- colnames(x))) {
    stop("x must contain column names")
  }
  formula <- as.formula(paste("y~", paste(nam, collapse = "+")))
  res <- imptree(formula, data = x, ...)
  call <- match.call()
  call[[1]] <- as.name("imptree")
  res$Call <- call
  res

} 

#
# The S3 registration of the two functions
#

imptree <- function(x, ...) {
  UseMethod("imptree")
}



#
# Function generating the a bag of imprecise classification trees, employing 'imptree' construct each single tree.
#
# Input:
#   formula:      Formula describing the strucutre (class variable ~ featutre variables)
#                     any interactions are ignored. Must be supplied!
#   data:         data.frame used to evaluate to given formula on. If not provided the
#                   the formula is evaluated on the calling environment
#   testdata:     data.frame to be employed as test set to measure accuracy. (Optional)
#   na.action:    What shall happen with 'NA's in the data (Default: 'na.pass')
#   control:      Named list controlling the growing and accuracy ouput
#                   Possible names are:
#                   - minbucket:    Minimal leaf size (Default: 1)
#                   - depth:        Positive integer limiting the tree to the given depth.
#                                     If not supplied tree is grown to maximal size (Default)
#                   - boot:         integer giving the number of trees in the bag (Default: 100)
#                   - calc.acc:     boolean; calculating accuracy on the 
#                                     training set for each tree (Default: FALSE)
#                   - calc.bag.acc: boolean; calculating accuracy on the training set
#                                     (Default: FALSE)
#                   - calc.oobe:    boolean; calculating the accuracy based on the 
#                                     out-of-bag-observations for each tree (Default: FALSE)
#   aggregation:  Which aggregation method should be used to calculate accuracy.
#                   Passed as parameter to call of 'accuracy' (Default: 'equal')
#                   For further information see function 'accuracy' (accuracy.r)
#   ...:          possibly futher arguments. Any arguments used in 'control' may be
#                   given here directly.
#
# Output:
#   Object of class 'impbag' as a named list
#     - Call: The functions call
#     - Bag:  A list containg the trees, grown on 'boot' bootstrapped data sets of the training data
#     - Greedy.tree: Single tree grown on the training data
#     - OoBAcc: average accuracy attained over all trees for out-of-bag observations
#                 (NULL if calc.oobe = FALSE)
#     - Accuracy: accuracy achieved on training data (NULL if calc.bag.acc = FALSE)
#     - Data: The training data used to construct the tree
#     - BIndices: List containing the indices of data used in each bootstrap sample
#     - formula: The formula describing the data structure
#     - Test: accuracy achieved on supplied test data (NULL if testdata = NULL)
#

impbag.formula <- function(formula, data = NULL, testdata = NULL, na.action = na.pass, 
                    control, gamma = 1, Tbase = 1, method = c("IDM", "NPI"), 
                    method.param, aggregation = c("equal", "dacc", "disjunction", "mean"), ...) {

# making sure a valid formula is supplied
  if(missing(formula) || !inherits(formula, "formula")) {
    stop("Please supply a valid formula")
  } else  {
# constructing a data.frame according to the supplied formula and na.action  
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "na.action"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    if(is.null(mf$na.action)) mf$na.action <- na.action
    mf[[1L]] <- as.name("model.frame")
    x <- eval(mf, parent.frame())
  }
# checking for the calculation method
  method <- match.arg(method)
# initializing the control arguements such as minbucket, depth, boot, calc.acc (for each single tree), 
# calc.bag.acc and calc.oobe. They may also be supplied via '...'
  if(missing(control)) {
    control <- NULL
  }
  controls <- imptree_control(controlList = control, Tbase = Tbase, gamma = gamma, ...)
# checking for the method specific parameters
  method.param <- (get(paste("checkParam", method, sep='.')))(method.param)
# storing the call
  call <- match.call()
# number of observations in global training set
  n <- NROW(x)
# generation of bootstrap samples (indices only)
  L.index <- lapply(1:controls$boot, function(b) sample(1:n, replace = TRUE))
  T.index <- lapply(L.index, function(i) setdiff(1:n, i))
# initializing list to store results of each bootstrap sample  
  bag <- list()
  oobe <- list()
  arr <- NULL
# iteration voer all bootstrap samples
  for(b in seq_len(controls$boot)) {
# growing a tree on a subset of training data, indetified by the attached indices
    bag[[b]] <- tree <- imptree(formula = formula, data = x[L.index[[b]], ], testdata = NULL, na.action = na.action, control = controls, gamma = gamma, Tbase = Tbase, method = method, method.param = method.param)
# calculation of accuracy on out-of-bag observations
    if(controls$calc.oobe) {
      oobe[[b]] <- accuracy(tree, data = x[T.index[[b]], ])$acc
    }
  }
# averaging over accuracy on the out-of-bag observations
  if(controls$calc.oobe) {    
    arr <- Reduce("+", lapply(oobe, function(i) {as.matrix(unlist(i))}))/length(oobe)
    dimnames(arr)[[2]] <- ""
  }
# generation of a single tree using the complete training set
  stree <- imptree(formula = formula, data = data, testdata = testdata, na.action = na.action, control = controls, gamma = gamma, Tbase = Tbase, method = method, method.param = method.param, ...)
# generating the structure for the 'impbag' object
  res <- list(Call = call, Bag = bag, Greedy.tree = stree, OoBAcc = arr, Accuracy = NULL, Data = x, BIndices = L.index, formula = formula(terms(formula, data = x)), Test = NULL)
# assigning class
  class(res) <- "impbag"
# calculating accuracy if specified
  if(controls$calc.bag.acc) res$Accuracy <- accuracy(res, aggmethod = aggregation, dominance = controls$dominance)
# assessing accuracy on test set if supplied
  if(!is.null(testdata)) {
    res$Test <- accuracy(res, data = testdata, aggmethod = aggregation, dominance = controls$dominance)
  }
  res
}

#
# Wrapping the impbag function for a non-formula case: y and x as arguments
#

impbag.default <- function(x, y, ...) {
  
  if(missing(y)|| missing(x)) {
    stop("Both y and x need to be supplied")
  }
  
  if(is.null(nam <- colnames(x))) {
    stop("x must contain column names")
  }
  formula <- as.formula(paste("y~", paste(nam, collapse = "+")))
  res <- impbag(formula, data = x, ...)
  call <- match.call()
  call[[1]] <- as.name("impbag")
  res$Call <- call
  res
  
} 

#
# The S3 registration of the two functions
#

impbag <- function(x, ...) {
  UseMethod("impbag")
}
