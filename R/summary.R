#' @title Classification with Imprecise Probabilities
#' 
#' @description Summary function for an imptree object, assesses 
#' accuracy achieved on training data and further tree properties.
#' 
#' @aliases summary.imptree print.summary.imptree
#' 
#' @param object An object of class \code{imptree}. See details.
#' @param dominance Dominace criterion to be applied when predicting
#' classes. This may either be \code{"strong"} (default) or
#' \code{"max"}. See details at \code{\link{predict.imptree}}.
#' @param utility Utility for the utility based accuracy measure for
#' a vacuous prediction result (default: 0.65).
#' @param \dots Further arguments are ignored at the moment.
#' 
#' @return A named list of class \code{summary.imptree} containing
#' the tree creation call, accuracy on the training data, meta data
#' and supplied the utility and dominance criterion for evaluation.
#' \item{call}{Call to create the tree}
#' \item{utility}{Supplied utility, or its default value}
#' \item{dominance}{Supplied dominace criterion, or its 
#' default value}
#' \item{sizes}{List containing the overall number and number of 
#' indeterminate predictions on training data}
#' \item{acc}{1-column matrix containing the accuracy measures 
#' on training data with nicer names (without size information)
#' (see \code{\link{predict.imptree}})}
#' \item{meta}{1-column matrix containing the tree's depth, 
#' number of leaves and number of nodes}
#' 
#' @details
#' An existence check on the stored C++ object reference is carried
#' out at first. If the reference is not valid the original call
#' for \code{"object"} is printed as error.
#' 
#' @author Paul Fink \email{Paul.Fink@@stat.uni-muenchen.de}
#' 
#' @seealso \code{\link{imptree}}, \code{\link{predict.imptree}},
#' for information on a single node \code{\link{node_imptree}}
#' 
#' @keywords tree
#' 
#' @importFrom stats predict
#' @export
summary.imptree <- function(object, utility = 0.65, 
                            dominance = c("strong", "max"), ...) {
  
  # Are the C++ object references still stored in the R object?
  tryCatch({hasRoot_cpp(object$tree)},  error = function(e) {
    stop(gettextf(
      "reference to tree is not valid; see element \"call\" of '%s' for recreation", 
                 deparse(substitute(object)), domain = "R-imptree"))
  })
  
  # get the tree information
  metaresult <- treeInformation_cpp(object$tree)
  dimnames(metaresult) <- list(c("Depth", "Leaves", "Nodes"),"")

  dominance <- match.arg(dominance)
  
  # prediction on training data
  trainresult <- predict(object, utility = utility, 
                         dominance = dominance)
  
  teval <- trainresult$evaluation
  
  meval <- matrix(unlist(teval)[-c(1,3)], ncol = 1)
  dimnames(meval) <- list(c("Determinacy",
                            "Average indeterminate size",
                            "Single-Set Accuracy",
                            "Set-Accuracy",
                            "Discounted Accuracy",
                            sprintf("%.2f utility based Accuracy",
                                     utility)),
                          "")
  res <- list(call = object$call,
              utility = utility,
              dominance = dominance,
              sizes = list(obs = teval$nObs, iobs = teval$nObsIndet),
              acc = meval,
              meta = metaresult)
  class(res) <- c("summary.imptree")
  res
}

#' @rdname summary.imptree
#' @method print summary.imptree
#' @param x an object of class \code{summary.imptree}
#' @return The printing function returns the
#' \code{summary.imptree} object invisibly.
#' @export
print.summary.imptree <- function(x, ...) {
  cat("\nCall:  ", paste(deparse(x$call),
                         sep = "\n", collapse = "\n"),
      "\n\n", sep = "")  
  cat(gettextf("%d observations in training data\n",
               as.integer(x$sizes$obs), domain = "R-imptree"))
  meta <- x$meta
  dimnames(meta) <- list(gettext("Depth", "Leaves", "Nodes", 
                                 domain ="R-imptree"),
                         "")
  print(meta)
  cat(gettextf(
    "\nAccuracy achieved on training data, based on '%s' dominance:\n",
    x$dominance, domain = "R-imptree"))
  if(x$sizes$iobs > 0) {
    cat(gettextf("\t%d indeterminate predictions\n", 
                 as.integer(x$sizes$iobs), domain = "R-imptree"))
  }
  acc <- x$acc
  
  dimnames(acc) <- list(gettextf(c("Determinacy", 
                                   "Average indeterminate size", 
                                   "Single-Set Accuracy", 
                                   "Set-Accuracy", 
                                   "Discounted Accuracy",
                                   "%.2f utility based Accuracy"),
                                 x$utility, 
                                 domain = "R-imptree"),
                        "")
  print(acc)
  invisible(x)
}
